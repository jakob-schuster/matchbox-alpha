use std::{fs::File, io::BufReader};

use noodles::sam;
use util::rev_comp;

use crate::{
    lang::ast::read::{FastaRead, FastqRead, SamRead},
    myers::VarMyers,
};

use super::*;

#[derive(Clone, Debug)]
pub enum FunctionError<'a> {
    UnknownFunction(Id),
    BadArgumentTypes(Id, Vec<&'a Val<'a>>),

    FileLoadError(String),
    BadCast(Id, String),
    EvalError(Rc<EvalError<'a>>),
}

pub fn apply<'a, 'b>(
    id: &Id,
    args: &'b [&'a Val],
    arena: &'a Arena,
) -> Result<&'a Val<'a>, FunctionError<'a>> {
    match &id[..] {
        // type casts
        "to_str" => {
            match args {
                // any primitive values can be printed

                // str values just go straight through
                [s @ Val::Str(_)] => Ok(s),

                [Val::Bool(b)] => Ok(match b {
                    true => arena.alloc(Val::Str(arena.alloc(String::from("true")))),
                    false => arena.alloc(Val::Str(arena.alloc(String::from("true")))),
                }),
                [Val::Num(n)] => Ok(arena.alloc(Val::Str(arena.alloc(n.to_string())))),
                [Val::Seq(s)] => Ok(arena.alloc(Val::Str(arena.alloc(util::seq_to_string(s))))),

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        "to_upper" => match args {
            [Val::Str(s)] => Ok(arena.alloc(Val::Str(arena.alloc(s.to_ascii_uppercase())))),

            // bad argument types!
            _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
        },

        "to_lower" => match args {
            [Val::Str(s)] => Ok(arena.alloc(Val::Str(arena.alloc(s.to_ascii_lowercase())))),

            // bad argument types!
            _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
        },

        "find" => match args {
            [Val::Str(s1), Val::Str(s2)] => Ok(arena.alloc(Val::Num(match s1.find(s2) {
                Some(i) => i as i32,
                None => -1,
            }))),

            // bad argument types!
            _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
        },

        "rfind" => match args {
            [Val::Str(s1), Val::Str(s2)] => Ok(arena.alloc(Val::Num(match s1.rfind(s2) {
                Some(i) => i as i32,
                None => -1,
            }))),

            // bad argument types!
            _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
        },

        "to_seq" => {
            match args {
                // seq values just go straight through
                [s @ Val::Seq(_)] => Ok(s),

                [Val::Str(s)] => {
                    if s.bytes()
                        .all(|b| matches!(b.to_ascii_uppercase(), b'A' | b'C' | b'G' | b'T'))
                    {
                        Ok(arena.alloc(Val::Seq(s.as_bytes())))
                    } else {
                        Err(FunctionError::BadCast(
                            (*s).to_string(),
                            String::from("Seq"),
                        ))
                    }
                }

                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        // length
        "length" | "len" => {
            match args {
                [Val::Str(s)] => Ok(arena.alloc(Val::Num(s.len() as i32))),
                [Val::Seq(s)] => Ok(arena.alloc(Val::Num(s.len() as i32))),

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        // minus / revcomp
        "minus" => {
            match args {
                [Val::Struct(sv)] => {
                    // first, take a slice of the sequence
                    let seq = match sv
                        .address("seq", arena)
                        .map_err(|e| FunctionError::EvalError(Rc::new(e)))?
                    {
                        Val::Seq(seq) => Ok(*seq),
                        v => Err(FunctionError::EvalError(Rc::new(EvalError::TypeError(
                            v.clone(),
                        )))),
                    }?;
                    let new_seq = arena.alloc(Val::Seq(arena.alloc(util::rev_comp(&seq))));

                    // take a slice of the quality score too, if appropriate
                    let new_sv = match sv.address("qual", arena) {
                        Ok(v) => match v {
                            Val::Str(q) => Ok(sv
                                .with_all(
                                    &[
                                        ("seq", new_seq),
                                        (
                                            "qual",
                                            arena.alloc(Val::Str(
                                                arena.alloc(
                                                    String::from_utf8(
                                                        q.as_bytes()
                                                            .iter()
                                                            .rev()
                                                            .cloned()
                                                            .collect_vec(),
                                                    )
                                                    .unwrap(),
                                                ),
                                            )),
                                        ),
                                    ],
                                    arena,
                                )
                                .map_err(|e| FunctionError::EvalError(Rc::new(e)))?),
                            _ => Err(FunctionError::EvalError(Rc::new(EvalError::TypeError(
                                v.clone(),
                            )))),
                        },

                        // there is no quality score
                        Err(_) => Ok(sv
                            .with_all(&[(&"seq", new_seq)], arena)
                            .map_err(|e| FunctionError::EvalError(Rc::new(e)))?),
                    }?;

                    Ok(arena.alloc(Val::Struct(new_sv)))
                }

                [Val::Seq(s)] => Ok(arena.alloc(Val::Seq(arena.alloc(util::rev_comp(s))))),

                [Val::Num(n)] => Ok(arena.alloc(Val::Num(-n))),

                [Val::Num(n1), Val::Num(n2)] => Ok(arena.alloc(Val::Num(n1 - n2))),

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        "append" => {
            match args {
                [Val::Seq(s1), Val::Seq(s2)] => Ok(arena.alloc(Val::Seq(
                    arena.alloc(s1.iter().chain(*s2).cloned().collect_vec()),
                ))),

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        "greater_than" => {
            match args {
                [Val::Num(n), Val::Num(m)] => Ok(arena.alloc(Val::Bool(*n > *m))),

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        "greater_than_or_equal_to" => {
            match args {
                [Val::Num(n), Val::Num(m)] => Ok(arena.alloc(Val::Bool(*n >= *m))),

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        "less_than" => {
            match args {
                [Val::Num(n), Val::Num(m)] => Ok(arena.alloc(Val::Bool(*n < *m))),

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        "less_than_or_equal_to" => {
            match args {
                [Val::Num(n), Val::Num(m)] => Ok(arena.alloc(Val::Bool(*n <= *m))),

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        "equal_to" => {
            match args {
                [Val::Num(n), Val::Num(m)] => Ok(arena.alloc(Val::Bool(*n == *m))),

                [Val::Str(s1), Val::Str(s2)] => Ok(arena.alloc(Val::Bool(**s1 == **s2))),

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        // plus
        "plus" => {
            match args {
                [Val::Num(n1), Val::Num(n2)] => Ok(arena.alloc(Val::Num(n1 + n2))),

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        "times" => {
            match args {
                [Val::Num(n1), Val::Num(n2)] => Ok(arena.alloc(Val::Num(n1 * n2))),

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        "modulo" => {
            match args {
                [Val::Num(n1), Val::Num(n2)] => Ok(arena.alloc(Val::Num(n1 % n2))),

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        "translate" => {
            match args {
                [Val::Seq(seq)] => Ok(arena.alloc(Val::Str(arena.alloc(util::translate(seq))))),

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        "describe" => {
            match args {
                [Val::Struct(StructVal::Read(r)), Val::List(l), Val::Num(edit_dist), Val::Bool(reverse_complement)] =>
                {
                    if let Val::Seq(read_seq) = r
                        .address("seq", arena)
                        .map_err(|_| FunctionError::BadArgumentTypes(id.clone(), args.to_vec()))?
                    {
                        // parse the struct, making sure its structure is good
                        let mut map = HashMap::new();
                        for v in l {
                            if let Val::Struct(s) = v {
                                if let (Ok(name_val), Ok(seq_val)) =
                                    (s.address("name", arena), s.address("seq", arena))
                                {
                                    if let Val::Str(n) = name_val {
                                        if let Val::Seq(seq) = match seq_val {
                                            Val::Str(s) => {
                                                apply(&"to_seq".to_string(), &[seq_val], arena)?
                                            }
                                            _ => seq_val,
                                        } {
                                            map.insert(*n, *seq);
                                        } else {
                                            return Err(FunctionError::BadArgumentTypes(
                                                id.clone(),
                                                args.to_vec(),
                                            ));
                                        }
                                    } else {
                                        return Err(FunctionError::BadArgumentTypes(
                                            id.clone(),
                                            args.to_vec(),
                                        ));
                                    }
                                } else {
                                    return Err(FunctionError::BadArgumentTypes(
                                        id.clone(),
                                        args.to_vec(),
                                    ));
                                }
                            } else {
                                return Err(FunctionError::BadArgumentTypes(
                                    id.clone(),
                                    args.to_vec(),
                                ));
                            }
                        }

                        // if necessary, add the reverse complements
                        let little_arena = Arena::new();
                        if *reverse_complement {
                            let mut new_map = map.clone();
                            for (id, seq) in &map {
                                new_map.insert(
                                    little_arena.alloc(format!("-{id}")),
                                    little_arena.alloc(rev_comp(seq)),
                                );
                            }
                            map = new_map;
                        }

                        // describe the read
                        let description = map
                            .iter()
                            .flat_map(|(id, seq)| {
                                VarMyers::new(seq)
                                    .find_all_disjoint(read_seq, *(edit_dist) as u8)
                                    .iter()
                                    .map(|matches| (id, *matches))
                                    .collect_vec()
                            })
                            .sorted_by_key(|(_, (start, _, _))| *start)
                            .map(|(id, _)| id)
                            .join(" _ ");

                        // allocate the description
                        Ok(arena.alloc(Val::Str(arena.alloc(if description.is_empty() {
                            String::from("[ _ ]")
                        } else {
                            format!("[ _ {description} _ ]")
                        }))))
                    } else {
                        Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec()))
                    }
                }

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        "slice" => {
            match args {
                [Val::Struct(sv), Val::Num(s), Val::Num(e)] => {
                    // first, take a slice of the sequence
                    let seq = match sv
                        .address("seq", arena)
                        .map_err(|e| FunctionError::EvalError(Rc::new(e)))?
                    {
                        Val::Seq(seq) => Ok(*seq),
                        v => Err(FunctionError::EvalError(Rc::new(EvalError::TypeError(
                            v.clone(),
                        )))),
                    }?;
                    let new_seq = arena.alloc(Val::Seq(&seq[*s as usize..*e as usize]));

                    // take a slice of the quality score too, if appropriate
                    let new_sv = match sv.address("qual", arena) {
                        Ok(v) => match v {
                            Val::Str(q) => Ok(sv
                                .with_all(
                                    &[
                                        ("seq", new_seq),
                                        (
                                            "qual",
                                            arena.alloc(Val::Str(&q[*s as usize..*e as usize])),
                                        ),
                                    ],
                                    arena,
                                )
                                .map_err(|e| FunctionError::EvalError(Rc::new(e)))?),
                            _ => Err(FunctionError::EvalError(Rc::new(EvalError::TypeError(
                                v.clone(),
                            )))),
                        },

                        // there is no quality score
                        Err(_) => Ok(sv
                            .with_all(&[("seq", new_seq)], arena)
                            .map_err(|e| FunctionError::EvalError(Rc::new(e)))?),
                    }?;

                    Ok(arena.alloc(Val::Struct(new_sv)))
                }

                [Val::Seq(seq), Val::Num(s), Val::Num(e)] => {
                    Ok(arena.alloc(Val::Seq(&seq[*s as usize..*e as usize])))
                }

                [Val::Str(string), Val::Num(s), Val::Num(e)] => {
                    Ok(arena.alloc(Val::Str(&string[*s as usize..*e as usize])))
                }

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        "tag" => {
            match args {
                [Val::Struct(sv), Val::Str(s)] => {
                    let desc = match sv.address("desc", arena) {
                        Ok(v) => match v {
                            Val::Str(s) => Ok(*s),
                            v => Err(FunctionError::EvalError(Rc::new(EvalError::TypeError(
                                v.clone(),
                            )))),
                        },
                        Err(_) => Ok(""),
                    }?;

                    let new_desc = arena.alloc(Val::Str(arena.alloc(format!("{} {}", desc, s))));

                    let new = sv
                        .with_all(&[("desc", new_desc)], arena)
                        .map_err(|e| FunctionError::EvalError(Rc::new(e)))?;
                    Ok(arena.alloc(Val::Struct(new)))
                }

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        "rename" => {
            match args {
                [Val::Struct(sv), s @ Val::Str(_)] => {
                    let new = sv
                        .with_all(&[("name", s)], arena)
                        .map_err(|e| FunctionError::EvalError(Rc::new(e)))?;
                    Ok(arena.alloc(Val::Struct(new)))
                }

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        // Return a new struct with all the same parameters as s1, but with the sequence (and if applicable, the quality score) from s2 appended on the end
        "concat" => match args {
            [Val::Struct(s1), Val::Struct(s2)] => {
                // Get sequences from both structs
                let seq1 = match s1
                    .address("seq", arena)
                    .map_err(|e| FunctionError::EvalError(Rc::new(e)))?
                {
                    Val::Seq(seq) => Ok(*seq),
                    v => Err(FunctionError::EvalError(Rc::new(EvalError::TypeError(
                        v.clone(),
                    )))),
                }?;

                let seq2 = match s2
                    .address("seq", arena)
                    .map_err(|e| FunctionError::EvalError(Rc::new(e)))?
                {
                    Val::Seq(seq) => Ok(*seq),
                    v => Err(FunctionError::EvalError(Rc::new(EvalError::TypeError(
                        v.clone(),
                    )))),
                }?;

                // Combine sequences
                let new_seq = arena.alloc(Val::Seq(
                    arena.alloc(seq1.iter().chain(seq2).cloned().collect_vec()),
                ));

                // If quality scores exist, combine those too
                let new_sv = match (s1.address("qual", arena), s2.address("qual", arena)) {
                    (Ok(Val::Str(q1)), Ok(Val::Str(q2))) => {
                        let new_qual = format!("{}{}", q1, q2);
                        s1.with_all(
                            &[
                                ("seq", new_seq),
                                ("qual", arena.alloc(Val::Str(arena.alloc(new_qual)))),
                            ],
                            arena,
                        )
                        .map_err(|e| FunctionError::EvalError(Rc::new(e)))?
                    }
                    _ => s1
                        .with_all(&[("seq", new_seq)], arena)
                        .map_err(|e| FunctionError::EvalError(Rc::new(e)))?,
                };

                Ok(arena.alloc(Val::Struct(new_sv)))
            }
            _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
        },

        "out" => {
            match args {
                [v] => Ok(arena.alloc(Val::Eff(v, handler::Handler::Stdout))),

                [v, Val::Handler(h)] => Ok(arena.alloc(Val::Eff(v, h.clone()))),

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }
        "send" => {
            match args {
                [Val::Handler(h), v] => Ok(arena.alloc(Val::Eff(v, h.clone()))),

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        // handlers
        "file" => {
            match args {
                [Val::Str(s)] => {
                    Ok(arena.alloc(Val::Handler(handler::Handler::File(s.to_string()))))
                }

                // bad argument types!
                _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
            }
        }

        // inputs
        "tsv" => match args {
            [Val::Str(s)] => {
                let mut rdr = csv::ReaderBuilder::new()
                    .delimiter(b'\t')
                    .from_path(Path::new(&s))
                    .map_err(|_| FunctionError::FileLoadError(s.to_string()))?;

                let field_names = rdr
                    .headers()
                    .iter()
                    .flat_map(|a| {
                        a.iter()
                            .map(String::from)
                            .map(|s| arena.alloc(s) as &String)
                    })
                    .collect_vec();

                let structs: Vec<_> = rdr
                    .records()
                    .map(|r| {
                        r.map(|sr| -> &Val<'_> {
                            arena.alloc(Val::Struct(StructVal::Regular {
                                map: HashMap::from_iter(sr.into_iter().enumerate().map(
                                    |(i, s)| {
                                        let field: &str = field_names.get(i).unwrap();
                                        let val: &Val =
                                            arena.alloc(Val::Str(arena.alloc(Id::from(s))));

                                        (field, val)
                                    },
                                )),
                            }))
                        })
                    })
                    .try_collect()
                    .map_err(|_| FunctionError::FileLoadError(s.to_string()))?;

                Ok(arena.alloc(Val::List(structs)))
            }

            _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
        },

        "csv" => match args {
            [Val::Str(s)] => {
                let mut rdr = csv::ReaderBuilder::new()
                    .from_path(Path::new(&s))
                    .map_err(|_| FunctionError::FileLoadError(s.to_string()))?;

                let field_names = rdr
                    .headers()
                    .iter()
                    .flat_map(|a| {
                        a.iter()
                            .map(String::from)
                            .map(|s| arena.alloc(s) as &String)
                    })
                    .collect_vec();

                let structs: Vec<_> = rdr
                    .records()
                    .map(|r| {
                        r.map(|sr| -> &Val<'_> {
                            arena.alloc(Val::Struct(StructVal::Regular {
                                map: HashMap::from_iter(sr.into_iter().enumerate().map(
                                    |(i, s)| {
                                        let field: &str = field_names.get(i).unwrap();
                                        let val: &Val =
                                            arena.alloc(Val::Str(arena.alloc(Id::from(s))));

                                        (field, val)
                                    },
                                )),
                            }))
                        })
                    })
                    .try_collect()
                    .map_err(|_| FunctionError::FileLoadError(s.to_string()))?;

                Ok(arena.alloc(Val::List(structs)))
            }

            _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
        },

        "fa" | "fasta" => match args {
            [Val::Str(s)] => {
                let f = bio::io::fasta::Reader::from_file(s)
                    .map_err(|e| FunctionError::FileLoadError(format!("{:?}", e)))?;

                let name_id = arena.alloc(Id::from("name"));
                let seq_id = arena.alloc(Id::from("seq"));

                let mut v = vec![];

                for rec in f.records() {
                    match rec {
                        Ok(read) => {
                            let seq = arena.alloc(read.seq().to_vec());
                            let name = arena.alloc(read.id().to_string());

                            let map: HashMap<&str, &Val<'_>> = HashMap::from([
                                (&name_id[..], arena.alloc(Val::Str(name)) as &Val),
                                (&seq_id[..], arena.alloc(Val::Seq(seq)) as &Val),
                            ]);

                            v.push(arena.alloc(Val::Struct(StructVal::Regular { map })) as &Val);
                        }
                        Err(_) => panic!(),
                    }
                }

                Ok(arena.alloc(Val::List(v)))
            }

            _ => Err(FunctionError::BadArgumentTypes(id.clone(), args.to_vec())),
        },

        _ =>
        // not a built-in function!
        {
            Err(FunctionError::UnknownFunction(id.clone()))
        }
    }
}
