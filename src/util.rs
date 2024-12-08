use std::collections::HashMap;
use std::fmt::Formatter;
use std::fs::File;
use std::hash::Hash;
use std::io::BufRead;
use std::path::Path;
use std::{fmt, io};

pub fn seq_to_string(seq: &[u8]) -> String {
    String::from_utf8(seq.to_vec()).expect("Bad seq!")
}

pub fn rev_comp(seq: &[u8]) -> Vec<u8> {
    bio::alphabets::dna::revcomp(seq)
}

#[derive(Clone, Default)]
pub struct Resource<K, V> {
    pub vals: HashMap<K, V>,
}

impl<K: fmt::Display, V: fmt::Display> fmt::Display for Resource<K, V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (key, val) in self.vals.iter() {
            write!(f, "{} : {}", key, val)?;
        }

        Ok(())
    }
}

impl<K: Eq + PartialEq + Hash + Clone, V: Clone> Resource<K, V> {
    pub fn new(vals: HashMap<K, V>) -> Self {
        Resource { vals }
    }

    pub fn get(&self, k: &K) -> &V {
        self.vals.get(k).expect("Couldn't find value in resource!")
    }

    pub fn with(&self, k: K, v: V) -> Self {
        let mut new_vals = self.vals.clone();
        new_vals.insert(k, v);

        Resource::new(new_vals)
    }
}

// taken from Rust by Example
pub fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Ran<T> {
    pub start: T,
    pub end: T,
}

impl<T: fmt::Display> fmt::Display for Ran<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}:{})", self.start, self.end)
    }
}

impl<T> Ran<T> {
    pub fn new(start: T, end: T) -> Self {
        Ran { start, end }
    }

    pub fn map<B>(&self, f: impl Fn(&T) -> B) -> Ran<B> {
        Ran::new(f(&self.start), f(&self.end))
    }

    pub fn to_tuple(&self) -> (&T, &T) {
        (&self.start, &self.end)
    }
}

impl<T: Ord> Ran<T> {
    pub fn disjoint(&self, other: &Self) -> bool {
        other.start >= self.end || self.start >= other.end
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Par<E, V> {
    Val(V),
    Exp(E),
}
impl<E: Clone, V: Copy> Par<E, V> {
    pub fn map(&self, f: impl Fn(&E) -> Par<E, V>) -> Par<E, V> {
        match self {
            Par::Val(v) => Par::Val(*v),
            Par::Exp(e) => f(e),
        }
    }

    pub fn try_map<R>(&self, f: impl Fn(&E) -> Result<Par<E, V>, R>) -> Result<Par<E, V>, R> {
        match self {
            Par::Val(v) => Ok(Par::Val(*v)),
            Par::Exp(e) => f(e),
        }
    }

    pub fn exp_map(&self, f: impl Fn(&E) -> E) -> Par<E, V> {
        match self {
            Par::Val(v) => Par::Val(*v),
            Par::Exp(e) => Par::Exp(f(e)),
        }
    }
}
impl<E: Clone, V: Clone> Par<E, V> {
    pub fn map_cl(&self, f: impl Fn(&E) -> Par<E, V>) -> Par<E, V> {
        match self {
            Par::Val(v) => Par::Val(v.clone()),
            Par::Exp(e) => f(e),
        }
    }

    pub fn try_map_cl<'a, R>(
        &self,
        f: impl Fn(&E) -> Result<Par<E, V>, R>,
    ) -> Result<Par<E, V>, R> {
        match self {
            Par::Val(v) => Ok(Par::Val(v.clone())),
            Par::Exp(e) => f(e),
        }
    }

    pub fn exp_map_cl(&self, f: impl Fn(&E) -> E) -> Par<E, V> {
        match self {
            Par::Val(v) => Par::Val(v.clone()),
            Par::Exp(e) => Par::Exp(f(e)),
        }
    }

    pub fn try_exp_map_cl<R>(&self, f: impl Fn(&E) -> Result<E, R>) -> Result<Par<E, V>, R> {
        match self {
            Par::Val(v) => Ok(Par::Val(v.clone())),
            Par::Exp(e) => f(e).map(|r| Par::Exp(r)),
        }
    }

    pub fn force<R>(&self, f: impl Fn(&E) -> Result<V, R>) -> Result<V, R> {
        match self {
            Par::Val(v) => Ok(v.clone()),
            Par::Exp(e) => f(e),
        }
    }
}

pub type Arena = bumpalo::Bump;

/// Translates a sequence into protein.
pub fn translate(seq: &[u8]) -> String {
    fn translate_codon(codon: &[u8]) -> char {
        match codon {
            b"TTT" => 'F',
            b"TTC" => 'F',
            b"TTA" => 'L',
            b"TTG" => 'L',
            b"TCT" => 'S',
            b"TCC" => 'S',
            b"TCA" => 'S',
            b"TCG" => 'S',
            b"TAT" => 'Y',
            b"TAC" => 'Y',
            b"TAA" => '-',
            b"TAG" => '-',
            b"TGT" => 'C',
            b"TGC" => 'C',
            b"TGA" => '-',
            b"TGG" => 'W',

            b"CTT" => 'L',
            b"CTC" => 'L',
            b"CTA" => 'L',
            b"CTG" => 'L',
            b"CCT" => 'P',
            b"CCC" => 'P',
            b"CCA" => 'P',
            b"CCG" => 'P',
            b"CAT" => 'H',
            b"CAC" => 'H',
            b"CAA" => 'Q',
            b"CAG" => 'Q',
            b"CGT" => 'R',
            b"CGC" => 'R',
            b"CGA" => 'R',
            b"CGG" => 'R',

            b"ATT" => 'I',
            b"ATC" => 'I',
            b"ATA" => 'I',
            b"ATG" => 'M',
            b"ACT" => 'T',
            b"ACC" => 'T',
            b"ACA" => 'T',
            b"ACG" => 'T',
            b"AAT" => 'N',
            b"AAC" => 'N',
            b"AAA" => 'K',
            b"AAG" => 'K',
            b"AGT" => 'S',
            b"AGC" => 'S',
            b"AGA" => 'R',
            b"AGG" => 'R',

            b"GTT" => 'V',
            b"GTC" => 'V',
            b"GTA" => 'V',
            b"GTG" => 'V',
            b"GCT" => 'A',
            b"GCC" => 'A',
            b"GCA" => 'A',
            b"GCG" => 'A',
            b"GAT" => 'D',
            b"GAC" => 'D',
            b"GAA" => 'E',
            b"GAG" => 'E',
            b"GGT" => 'G',
            b"GGC" => 'G',
            b"GGA" => 'G',
            b"GGG" => 'G',

            _ => '?',
        }
    }

    if seq.len() < 3 {
        // terminate on the end of sequences
        String::new()
    } else {
        // or, recursively call
        format!("{}{}", translate_codon(&seq[..3]), translate(&seq[3..]))
    }
}
