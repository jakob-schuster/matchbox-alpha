use core::str;
use std::{
    fmt::Display,
    fs::File,
    io::{BufWriter, Write},
    ops::Deref,
    os::unix::ffi::OsStrExt,
    path::Path,
};

use crate::util::seq_to_string;

use super::*;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Handler {
    File(String),
    Stdout,
    Counts,
    Average,
}

impl Display for Handler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Handler::File(filename) => format!("file({})", filename).fmt(f),
            Handler::Stdout => "stdout".fmt(f),
            Handler::Counts => "stats".fmt(f),
            Handler::Average => "average".fmt(f),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Eff {
    message: Arc<OwnedVal>,
    handler: handler::Handler,
}
impl Eff {
    pub fn new(message: Arc<OwnedVal>, handler: handler::Handler) -> Eff {
        Eff { message, handler }
    }
}

#[derive(Default)]
pub struct EffectHandler {
    file_handler: FileHandler,
    stats_handler: CountsHandler,
    average_handler: AverageHandler,
}

impl EffectHandler {
    pub fn handle(&mut self, eff: &Eff) {
        match &eff.handler {
            Handler::File(filename) => self.file_handler.handle_val(filename, eff.message.deref()),
            Handler::Stdout => println!("{}", eff.message),
            Handler::Counts => self.stats_handler.exec(&eff.message),
            Handler::Average => {
                if let OwnedVal::Num(num) = *eff.message {
                    self.average_handler.exec(num)
                }
            }
        }
    }

    pub fn finish(&self) {
        self.stats_handler.print();
        self.average_handler.print();
    }
}

#[derive(Default)]
struct FileHandler {
    files: HashMap<String, BufWriter<File>>,
    types: HashMap<String, FileType>,
}

enum FileType {
    Text,
    Fasta,
    Fastq,
    Sam,
}

impl FileHandler {
    fn type_from_filename(filename: &str) -> FileType {
        match Path::new(filename).extension() {
            Some(s) => match s.as_bytes() {
                b"fq" | b"fastq" => FileType::Fastq,
                b"fa" | b"fasta" => FileType::Fasta,
                b"sam" => FileType::Sam,
                _ => FileType::Text,
            },
            None => FileType::Text,
        }
    }

    pub fn handle_val(&mut self, filename: &str, val: &OwnedVal) {
        if let (Some(file), Some(t)) = (self.files.get_mut(filename), self.types.get(filename)) {
            match t {
                FileType::Text => {
                    file.write_all(format!("{}\n", val).as_bytes())
                        .expect("Couldn't write to file!");
                    // if let OwnedVal::Str(string) = val {
                    //     // file exists, just write to it
                    //     file.write_all(string.as_bytes())
                    //         .expect("Couldn't write to file!");
                    // } else {
                    //     panic!("Trying to write {:?} to a text file!", val)
                    // }
                }
                FileType::Fasta => {
                    let (head, seq) = match val {
                        OwnedVal::Read(r) => match r {
                            OwnedRead::Seq { seq: _ } => {
                                panic!("Trying to write seq to a fasta file!")
                            }
                            OwnedRead::Fasta { id, desc, seq } => (&format!("{id} {desc}"), seq),
                            OwnedRead::Fastq { id, seq, desc, .. } => {
                                (&format!("{id} {desc}"), seq)
                            }
                            OwnedRead::Sam { qname, seq, .. } => (qname, seq),
                        },
                        OwnedVal::Struct(s) => match (s.get("id"), s.get("desc"), s.get("seq")) {
                            (Some(id_arc), Some(desc_arc), Some(seq_arc)) => {
                                match (id_arc.as_ref(), desc_arc.as_ref(), seq_arc.as_ref()) {
                                    (
                                        OwnedVal::Str(id),
                                        OwnedVal::Str(desc),
                                        OwnedVal::Seq(seq),
                                    ) => (&format!("{id} {desc}"), seq),
                                    _ => panic!("Bad fields!"),
                                }
                            }
                            _ => todo!(),
                        },
                        _ => todo!(),
                    };

                    file.write_all(format!(">{}\n{}\n", head, seq_to_string(seq)).as_bytes())
                        .expect("Couldn't write to file!")
                }
                FileType::Fastq => {
                    let (head, seq, qual) = match val {
                        OwnedVal::Read(r) => match r {
                            OwnedRead::Seq { .. } => panic!("Not enough information!"),
                            OwnedRead::Fasta { .. } => panic!("Not enough information!"),
                            OwnedRead::Fastq {
                                id,
                                desc,
                                seq,
                                qual,
                            } => (&format!("{} {}", id, desc), seq, qual),
                            OwnedRead::Sam {
                                qname,
                                flag,
                                rname,
                                pos,
                                mapq,
                                cigar,
                                rnext,
                                pnext,
                                tlen,
                                seq,
                                qual,
                            } => (qname, seq, qual),
                        },
                        OwnedVal::Struct(s) => {
                            match (s.get("id"), s.get("desc"), s.get("seq"), s.get("qual")) {
                                (Some(id_arc), Some(desc_arc), Some(seq_arc), Some(qual_arc)) => {
                                    match (
                                        id_arc.as_ref(),
                                        desc_arc.as_ref(),
                                        seq_arc.as_ref(),
                                        qual_arc.as_ref(),
                                    ) {
                                        (
                                            OwnedVal::Str(id),
                                            OwnedVal::Str(desc),
                                            OwnedVal::Seq(seq),
                                            OwnedVal::Str(qual),
                                        ) => (&format!("{id} {desc}"), seq, qual),
                                        _ => panic!("Bad fields on {:?}!", s),
                                    }
                                }
                                _ => todo!(),
                            }
                        }
                        _ => panic!("Bad type!"),
                    };

                    file.write_all(
                        format!("@{}\n{}\n+\n{}\n", head, seq_to_string(seq), qual).as_bytes(),
                    )
                    .expect("Couldn't write to file!")
                }
                FileType::Sam => {
                    let (qname, flag, rname, pos, mapq, cigar, rnext, pnext, tlen, seq, qual) =
                        match val {
                            OwnedVal::Read(OwnedRead::Sam {
                                qname,
                                flag,
                                rname,
                                pos,
                                mapq,
                                cigar,
                                rnext,
                                pnext,
                                tlen,
                                seq,
                                qual,
                            }) => (
                                qname,
                                flag,
                                rname.as_str(),
                                pos,
                                mapq,
                                cigar.as_str(),
                                rnext.as_str(),
                                pnext,
                                tlen,
                                seq,
                                qual,
                            ),
                            OwnedVal::Read(OwnedRead::Fastq {
                                id: name,
                                seq,
                                qual,
                                ..
                            }) => (name, &4, "*", &0, &0, "*", "*", &0, &0, seq, qual),
                            _ => panic!("Bad type!"),
                        };

                    file.write_all(
                        format!(
                            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n",
                            qname,
                            flag,
                            rname,
                            pos,
                            mapq,
                            cigar,
                            rnext,
                            pnext,
                            tlen,
                            seq_to_string(seq),
                            qual
                        )
                        .as_bytes(),
                    )
                    .expect("Couldn't write to file!")
                }
            }
        } else {
            // file needs to be created
            if let Ok(file) = File::create(filename) {
                // now, write to it!
                let t = Self::type_from_filename(filename);
                // and add to the list
                self.files
                    .insert(String::from(filename), BufWriter::new(file));
                self.types.insert(String::from(filename), t);

                // then, handle it!
                self.handle_val(filename, val);
            } else {
                panic!("File {} couldn't be created!", filename)
            }
        }
    }
}

#[derive(Default, PartialEq)]
struct CountsHandler {
    map: HashMap<String, i32>,
}

impl CountsHandler {
    fn exec(&mut self, val: &OwnedVal) {
        let string = format!("{}", val);

        if let Some(count) = self.map.get(&string) {
            self.map.insert(string, count + 1);
        } else {
            self.map.insert(string, 1);
        }
    }

    fn print(&self) {
        if !self.eq(&Self::default()) {
            println!(": --- Counted Values --- :");
            for (key, val) in self.map.iter().sorted_by_key(|(_, val)| **val) {
                println!("{val}\t{key}");
            }
        }
    }
}

#[derive(Default, PartialEq)]
struct AverageHandler {
    count: i32,
    mean: f32,
    m2: f32,
}

impl AverageHandler {
    fn exec(&mut self, num: i32) {
        self.count += 1;
        let delta = num as f32 - self.mean;
        self.mean += delta / self.count as f32;
        let delta2 = num as f32 - self.mean;
        self.m2 += delta * delta2;
    }

    fn print(&self) {
        if !self.eq(&Self::default()) {
            println!(
                "Average: {:.2} ± {:.2}",
                self.mean,
                (self.m2 / self.count as f32).sqrt()
            );
        }
    }
}
