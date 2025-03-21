use crate::util::Arena;

use super::core::eval::EvalError;
use super::core::Val;
use ::core::str;

use std::{
    fmt::Debug,
    io::{BufRead, Error},
};

use super::*;
use indicatif::{ProgressBar, ProgressDrawTarget, ProgressStyle};
use noodles::sam::record::{Cigar, Data, QualityScores, Sequence};
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use seq_io::fasta::Record as FastaRecord;
use seq_io::fastq::Record as FastqRecord;

/// A read from any accepted input file type.
/// Differently stored and accessed depending on parsing backend.
#[derive(Eq, PartialEq)]
pub enum Read<'a> {
    Fasta(FastaRead<'a>),
    Fastq(FastqRead<'a>),
    Sam(SamRead<'a>),
    Seq(&'a [u8]),

    PairedEnd { r1: &'a Read<'a>, r2: &'a Read<'a> },
}

impl<'a> Debug for Read<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Read::Fasta(arg0) => "Fasta".fmt(f),
            Read::Fastq(arg0) => "Fastq".fmt(f),
            Read::Sam(arg0) => "Sam".fmt(f),
            Read::Seq(arg0) => "Seq".fmt(f),
            Read::PairedEnd { r1, r2 } => "PairedEndFastq".fmt(f),
        }
    }
}

impl<'a: 'b, 'b> Read<'a> {
    pub fn to_val(&self) -> core::Val {
        core::Val::Struct(core::StructVal::Read(self))
    }

    pub fn to_regular_structval(
        &'a self,
        arena: &'b Arena,
    ) -> Result<HashMap<&'b str, &'b Val<'b>>, EvalError<'a>> {
        let f = |s: &'b str| -> Result<(&'b str, &'b Val<'b>), EvalError<'a>> {
            Ok((arena.alloc(s), self.address(s, arena)?))
        };

        let g = |s: &[&'b str]| -> Result<HashMap<&'b str, &'b Val<'b>>, EvalError<'a>> {
            s.iter()
                .map(|s0| f(s0))
                .collect::<Result<HashMap<_, _>, _>>()
        };

        // get all the fields (this is going to be a bit weird..)
        match self {
            Read::Seq(_) => g(&["seq"]),
            Read::Fasta(_) => g(&["seq", "id", "desc"]),
            Read::Fastq(_) => g(&["seq", "id", "desc", "qual"]),
            Read::Sam(_) => g(&[
                "qname", "flag", "rname", "pos", "mapq", "rnext", "pnext", "tlen", "seq", "qual",
            ]),
            Read::PairedEnd { r1, r2 } => g(&["r1", "r2"]),
        }
    }

    pub fn address(
        &'a self,
        id: &str,
        arena: &'b Arena,
    ) -> Result<&'b core::Val<'b>, EvalError<'a>> {
        match self {
            Read::Fasta(fasta_read) => match id {
                "seq" => Ok(arena.alloc(core::Val::Seq(fasta_read.seq()))),
                "id" => Ok(arena.alloc(core::Val::Str(fasta_read.id()))),
                "desc" => Ok(arena.alloc(core::Val::Str(fasta_read.desc()))),
                _ => Err(EvalError::ReadFieldError),
            },
            Read::Fastq(fastq_read) => match id {
                "seq" => Ok(arena.alloc(core::Val::Seq(fastq_read.seq()))),
                "id" => Ok(arena.alloc(core::Val::Str(fastq_read.id()))),
                "desc" => Ok(arena.alloc(core::Val::Str(fastq_read.desc()))),
                "qual" => Ok(arena.alloc(core::Val::Str(
                    arena.alloc(str::from_utf8(fastq_read.qual()).unwrap()),
                ))),
                _ => Err(EvalError::ReadFieldError),
            },
            Read::Sam(sam_read) => match id {
                "qname" | "name" | "id" => {
                    Ok(arena.alloc(core::Val::Str(str::from_utf8(sam_read.qname()).unwrap())))
                }
                "flag" => Ok(arena.alloc(core::Val::Num(sam_read.flag() as i32))),
                "rname" | "reference_name" => {
                    Ok(arena.alloc(core::Val::Str(str::from_utf8(sam_read.rname()).unwrap())))
                }
                "pos" | "start" => Ok(arena.alloc(core::Val::Num(sam_read.pos() as i32))),
                "mapq" => Ok(arena.alloc(core::Val::Num(sam_read.mapq() as i32))),
                "rnext" | "mate_name" => {
                    Ok(arena.alloc(core::Val::Str(str::from_utf8(sam_read.rnext()).unwrap())))
                }
                "pnext" => Ok(arena.alloc(core::Val::Num(sam_read.pnext() as i32))),
                "tlen" | "template_length" => Ok(arena.alloc(core::Val::Num(sam_read.tlen()))),
                "seq" => Ok(arena.alloc(core::Val::Seq(sam_read.seq()))),
                "qual" => Ok(arena.alloc(core::Val::Str(str::from_utf8(sam_read.qual()).unwrap()))),

                _ => Err(EvalError::ReadFieldError),
            },
            Read::Seq(s) => match id {
                "seq" => Ok(arena.alloc(core::Val::Seq(s))),
                _ => Err(EvalError::ReadFieldError),
            },
            Read::PairedEnd { r1, r2 } => match id {
                "r1" => Ok(arena.alloc(Val::Struct(core::StructVal::Read(r1)))),
                "r2" => Ok(arena.alloc(Val::Struct(core::StructVal::Read(r2)))),
                _ => Err(EvalError::ReadFieldError),
            },
        }
    }
}

pub enum FastaRead<'a> {
    SeqIOFasta(seq_io::fasta::RefRecord<'a>),
    BioFasta(&'a bio::io::fasta::Record),
    Synthetic {
        seq: &'a [u8],
        id: &'a str,
        desc: &'a str,
    },
}

impl<'a> PartialEq for FastaRead<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id() && self.seq() == other.seq()
    }
}

impl<'a> Eq for FastaRead<'a> {}

impl<'a> FastaRead<'a> {
    pub fn new(id: &'a str, desc: &'a str, seq: &'a [u8]) -> FastaRead<'a> {
        FastaRead::Synthetic { id, desc, seq }
    }

    /// Gets the seq of a read.
    pub fn seq(&self) -> &[u8] {
        match self {
            FastaRead::SeqIOFasta(fasta) => fasta.seq(),
            FastaRead::BioFasta(fasta) => fasta.seq(),
            FastaRead::Synthetic { seq, .. } => seq,
        }
    }

    /// Gets the name of a read.
    pub fn id(&'a self) -> &'a str {
        match self {
            FastaRead::SeqIOFasta(fasta) => fasta.id().ok().unwrap_or(""),
            FastaRead::BioFasta(fasta) => fasta.id(),
            FastaRead::Synthetic { id, .. } => id,
        }
    }

    /// Gets the description line of a read.
    pub fn desc(&'a self) -> &'a str {
        match self {
            FastaRead::SeqIOFasta(fasta) => fasta
                .desc()
                .expect("Couldn't read description!")
                .unwrap_or_default(),
            FastaRead::BioFasta(fasta) => fasta.desc().unwrap_or_default(),
            FastaRead::Synthetic { desc, .. } => desc,
        }
    }

    // pub fn to_val(&self) -> core::Val {
    //     core::Val::Read(core::Read::Fasta {
    //         name: self.name(),
    //         seq: self.seq(),
    //     })
    // }
}

pub enum FastqRead<'a> {
    SeqIOFastq(seq_io::fastq::RefRecord<'a>),
    BioFastq(&'a bio::io::fastq::Record),
    Synthetic {
        seq: &'a [u8],
        id: &'a str,
        desc: &'a str,
        qual: &'a [u8],
    },
}

impl<'a> PartialEq for FastqRead<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id() && self.seq() == other.seq() && self.qual() == other.qual()
    }
}

impl<'a> Eq for FastqRead<'a> {}

impl<'a> FastqRead<'a> {
    pub fn new(seq: &'a [u8], id: &'a str, desc: &'a str, qual: &'a [u8]) -> FastqRead<'a> {
        FastqRead::Synthetic {
            seq,
            id,
            desc,
            qual,
        }
    }

    /// Gets the seq of a read.
    pub fn seq(&self) -> &[u8] {
        match self {
            FastqRead::SeqIOFastq(fastq) => fastq.seq(),
            FastqRead::BioFastq(fastq) => fastq.seq(),
            FastqRead::Synthetic { seq, .. } => seq,
        }
    }

    /// Gets the id of a read.
    pub fn id(&self) -> &str {
        match self {
            FastqRead::SeqIOFastq(fastq) => fastq.id().ok().unwrap_or_default(),
            FastqRead::BioFastq(fastq) => fastq.id(),
            FastqRead::Synthetic { id, .. } => id,
        }
    }

    /// Gets the description line of a read.
    pub fn desc(&self) -> &str {
        match self {
            FastqRead::SeqIOFastq(fastq) => fastq
                .desc()
                .expect("Couldn't read description!")
                .unwrap_or_default(),
            FastqRead::BioFastq(fastq) => fastq.desc().unwrap_or_default(),
            FastqRead::Synthetic { desc, .. } => desc,
        }
    }

    /// Gets the quality string of a read.
    pub fn qual(&self) -> &[u8] {
        match self {
            FastqRead::SeqIOFastq(fastq) => fastq.qual(),
            FastqRead::BioFastq(fastq) => fastq.qual(),
            FastqRead::Synthetic { qual, .. } => qual,
        }
    }
}

pub enum SamRead<'a> {
    // Sam(rust_htslib::bam::Record),
    Noodles {
        record: &'a noodles::sam::Record,
        cigar: &'a noodles::sam::record::Cigar<'a>,
        seq: &'a noodles::sam::record::Sequence<'a>,
        qual: &'a noodles::sam::record::QualityScores<'a>,
        data: &'a noodles::sam::record::Data<'a>,
    },
    Synthetic {
        qname: &'a [u8],
        flag: u16,
        rname: &'a [u8],
        pos: i64,
        mapq: u8,
        cigar: &'a [u8],
        rnext: &'a [u8],
        pnext: i64,
        tlen: i32,
        seq: &'a [u8],
        qual: &'a [u8],
        tags: &'a [u8],
    },
}

impl<'a> PartialEq for SamRead<'a> {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

impl<'a> Eq for SamRead<'a> {}

impl<'a> SamRead<'a> {
    pub fn new(
        qname: &'a [u8],
        flag: u16,
        rname: &'a [u8],
        pos: i64,
        mapq: u8,
        cigar: &'a [u8],
        rnext: &'a [u8],
        pnext: i64,
        tlen: i32,
        seq: &'a [u8],
        qual: &'a [u8],
        tags: &'a [u8],
    ) -> SamRead<'a> {
        SamRead::Synthetic {
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
            tags,
        }
    }

    pub fn qname(&self) -> &'a [u8] {
        match self {
            SamRead::Noodles { record, .. } => record.name().unwrap_or_default(),
            SamRead::Synthetic { qname, .. } => qname,
        }
    }

    pub fn flag(&self) -> u16 {
        match self {
            SamRead::Noodles { record, .. } => record.flags().unwrap_or_default().bits(),
            SamRead::Synthetic { flag, .. } => *flag,
        }
    }

    pub fn rname(&self) -> &'a [u8] {
        match self {
            SamRead::Noodles { record, .. } => match record.reference_sequence_name() {
                Some(a) => a,
                None => b"*",
            },
            SamRead::Synthetic { rname, .. } => rname,
        }
    }

    pub fn pos(&self) -> i64 {
        match self {
            SamRead::Noodles { record, .. } => match record.alignment_start() {
                Some(pos) => pos.unwrap().get() as i64,
                None => 0,
            },
            SamRead::Synthetic { pos, .. } => *pos,
        }
    }

    pub fn mapq(&self) -> u8 {
        match self {
            SamRead::Noodles { record, .. } => match record.mapping_quality() {
                Some(a) => a.unwrap().get(),
                None => 0,
            },
            SamRead::Synthetic { mapq, .. } => *mapq,
        }
    }

    pub fn cigar(&self) -> &'a [u8] {
        match self {
            SamRead::Noodles { cigar, .. } => match cigar.is_empty() {
                false => {
                    let f = |cigar: &'a Cigar| -> &'a [u8] { cigar.as_ref() };
                    f(cigar)
                }
                true => b"*",
            },
            SamRead::Synthetic { cigar, .. } => cigar,
        }
    }

    pub fn rnext(&self) -> &'a [u8] {
        match self {
            SamRead::Noodles { record, .. } => match record.mate_reference_sequence_name() {
                Some(a) => a,
                None => b"*",
            },
            SamRead::Synthetic { rnext, .. } => rnext,
        }
    }

    pub fn pnext(&self) -> i64 {
        match self {
            SamRead::Noodles { record, .. } => match record.mate_alignment_start() {
                Some(a) => a.unwrap().get() as i64,
                None => 0,
            },
            SamRead::Synthetic { pnext, .. } => *pnext,
        }
    }

    pub fn tlen(&self) -> i32 {
        match self {
            SamRead::Noodles { record, .. } => record.template_length().unwrap_or_default(),
            SamRead::Synthetic { tlen, .. } => *tlen,
        }
    }

    pub fn seq(&self) -> &'a [u8] {
        match self {
            SamRead::Noodles { seq, .. } => {
                let f = |seq: &'a Sequence| -> &'a [u8] { seq.as_ref() };
                f(seq)
            }
            SamRead::Synthetic { seq, .. } => seq,
        }
    }

    pub fn qual(&self) -> &'a [u8] {
        match self {
            SamRead::Noodles { qual, .. } => {
                let f = |quals: &'a QualityScores<'a>| -> &'a [u8] { quals.as_ref() };
                f(qual)
            }
            SamRead::Synthetic { qual, .. } => qual,
        }
    }

    pub fn tags(&self) -> &'a [u8] {
        match self {
            SamRead::Noodles { data, .. } => {
                let f = |data: &'a Data<'a>| -> &'a [u8] { data.as_ref() };
                f(data)
            }
            SamRead::Synthetic { tags, .. } => tags,
        }
    }
}

pub struct ReaderWrapped {
    bar: ProgressBar,
    reader: Reader,
}

impl ReaderWrapped {
    pub fn new_paired(
        filename1: &str,
        filename2: &str,
        parallel_chunk_size: usize,
        take_first: Option<usize>,
    ) -> Result<ReaderWrapped, InputError> {
        let (filetype1, bufread1) = reader(filename1)?;
        let (filetype2, bufread2) = reader(filename2)?;

        // make an estimate
        let estimate = ReaderWrapped::estimate_reads(filename1, 10000000, &filetype1)
            .map_err(InputError::FileOpenError)?;

        // make the bar
        let style = ProgressStyle::with_template(
            "{prefix} {elapsed_precise} [{bar:40.red/yellow}] {percent}% {msg}",
        )
        .unwrap()
        .progress_chars(" @=");
        let bar = ProgressBar::new(estimate)
            .with_style(style)
            .with_prefix(String::from(filename1));
        bar.set_draw_target(ProgressDrawTarget::stderr());

        match (filetype1, filetype2) {
            (FileType::Fasta, FileType::Fasta) => Ok(ReaderWrapped {
                bar,
                reader: Reader::BioPairedEndFasta {
                    buffer1: bufread1,
                    buffer2: bufread2,
                    parallel_chunk_size,
                    take_first,
                },
            }),
            (FileType::Fastq, FileType::Fastq) => Ok(ReaderWrapped {
                bar,
                reader: Reader::BioPairedEndFastq {
                    buffer1: bufread1,
                    buffer2: bufread2,
                    parallel_chunk_size,
                    take_first,
                },
            }),
            _ => Err(InputError::FileTypeError(
                FileTypeError::UnsupportedPairedCombination(
                    "can only pair two FASTQs or two FASTAs!".to_string(),
                ),
            )),
        }
    }

    pub fn new(
        filename: &str,
        parallel_chunk_size: usize,
        take_first: Option<usize>,
    ) -> Result<ReaderWrapped, InputError> {
        let (filetype, bufread) = reader(filename)?;

        // make an estimate
        let estimate = ReaderWrapped::estimate_reads(filename, 1000000, &filetype)
            .map_err(InputError::FileOpenError)?;

        // make the bar
        let style = ProgressStyle::with_template(
            "{prefix} {elapsed_precise} [{bar:40.red/yellow}] {percent}% {msg}",
        )
        .unwrap()
        .progress_chars(" @=");
        let bar = ProgressBar::new(estimate)
            .with_style(style)
            .with_prefix(String::from(filename));
        bar.set_draw_target(ProgressDrawTarget::stderr());

        // just use bio parsers for now; can't use seq_io in parallel on gzipped files
        let reader = Reader::from_bufread(bufread, &filetype, parallel_chunk_size, take_first);

        Ok(ReaderWrapped { bar, reader })
    }

    pub fn map<T: Send, R>(
        self,
        local_fn: impl Fn(Read) -> T + Sync,
        global_fn: impl Fn(T, &mut R),
        global_data: &mut R,
    ) {
        self.reader.map(local_fn, global_fn, global_data, &self.bar);

        self.bar.finish_with_message("Done!");
    }

    fn estimate_reads(filename: &str, buffer_len: u64, filetype: &FileType) -> Result<u64, Error> {
        use std::io::Read;

        let f = File::open(filename)?;

        // get the length of the file in bytes
        let file_len = f.metadata()?.len();

        // establish a buffer of buffer_len bytes
        let b = BufReader::new(f).take(buffer_len);

        // read in that buffer and count the records you find
        let count = Reader::from_bufread(Box::new(b), filetype, 1, None).count();

        // calculate the estimate
        let estimate = (count as f32 * file_len as f32 / buffer_len as f32) as u64;

        Ok(estimate)
    }
}

/// A reader for input files, taking either Fastq or Fasta files.
/// Currently, only the rust-bio parser backend is used.
pub enum Reader {
    BioFasta {
        buffer: Box<dyn BufRead>,
        parallel_chunk_size: usize,
        take_first: Option<usize>,
    },
    BioFastq {
        buffer: Box<dyn BufRead>,
        parallel_chunk_size: usize,
        take_first: Option<usize>,
    },
    BioFastqSingleThreaded {
        buffer: Box<dyn BufRead>,
        take_first: Option<usize>,
    },
    BioFastaSingleThreaded {
        buffer: Box<dyn BufRead>,
        take_first: Option<usize>,
    },
    BioPairedEndFastq {
        buffer1: Box<dyn BufRead>,
        buffer2: Box<dyn BufRead>,
        parallel_chunk_size: usize,
        take_first: Option<usize>,
    },
    BioPairedEndFasta {
        buffer1: Box<dyn BufRead>,
        buffer2: Box<dyn BufRead>,
        parallel_chunk_size: usize,
        take_first: Option<usize>,
    },

    NoodlesSam {
        buffer: Box<dyn BufRead>,
        parallel_chunk_size: usize,
        take_first: Option<usize>,
    },

    SeqList {
        buffer: Box<dyn BufRead>,
        parallel_chunk_size: usize,
        take_first: Option<usize>,
    },
}

impl Reader {
    /// Creates a new reader, given the filename,
    /// the size of each parallel-processed chunk of reads,
    /// and the option to only take the first n reads.
    pub fn new(
        filename: &str,
        parallel_chunk_size: usize,
        take_first: Option<usize>,
    ) -> Result<Reader, InputError> {
        let (filetype, bufread) = reader(filename)?;

        // just use bio parsers for now; can't use seq_io in parallel on gzipped files
        Ok(Reader::from_bufread(
            bufread,
            &filetype,
            parallel_chunk_size,
            take_first,
        ))
    }

    pub fn from_bufread(
        buffer: Box<dyn BufRead>,
        filetype: &FileType,
        parallel_chunk_size: usize,
        take_first: Option<usize>,
    ) -> Reader {
        match filetype {
            FileType::Fasta => Reader::BioFasta {
                buffer,
                parallel_chunk_size,
                take_first,
            },
            // FileType::Fasta => Reader::BioFastaSingleThreaded(bufread, take_first),
            FileType::Fastq => Reader::BioFastq {
                buffer,
                parallel_chunk_size,
                take_first,
            },
            // FileType::Fastq => Reader::BioFastqSingleThreaded(bufread, take_first),
            FileType::List => Reader::SeqList {
                buffer,
                parallel_chunk_size,
                take_first,
            },

            FileType::Sam => Reader::NoodlesSam {
                buffer,
                parallel_chunk_size,
                take_first,
            },
            FileType::Bam => todo!(),
        }
    }

    pub fn count(&mut self) -> usize {
        match self {
            Reader::BioFasta { buffer, .. } => bio::io::fasta::Reader::from_bufread(buffer)
                .records()
                .count(),
            Reader::BioFastq { buffer, .. } => bio::io::fastq::Reader::from_bufread(buffer)
                .records()
                .count(),
            Reader::BioFastqSingleThreaded { buffer, .. } => {
                bio::io::fastq::Reader::from_bufread(buffer)
                    .records()
                    .count()
            }

            Reader::BioFastaSingleThreaded { buffer, .. } => {
                bio::io::fasta::Reader::from_bufread(buffer)
                    .records()
                    .count()
            }
            Reader::SeqList { buffer, .. } => buffer.lines().count(),
            Reader::NoodlesSam { buffer, .. } => {
                let mut reader = noodles::sam::io::Reader::new(buffer);
                let header = reader.read_header();
                reader.records().count()
            }
            Reader::BioPairedEndFastq {
                buffer1,
                buffer2,
                parallel_chunk_size,
                take_first,
            } => bio::io::fastq::Reader::from_bufread(buffer1)
                .records()
                .count(),
            Reader::BioPairedEndFasta {
                buffer1,
                buffer2,
                parallel_chunk_size,
                take_first,
            } => bio::io::fasta::Reader::from_bufread(buffer1)
                .records()
                .count(),
        }
    }

    /// Maps a function across all the reads in the input.
    pub fn map<T: Send, R>(
        self,
        local_fn: impl Fn(Read) -> T + Sync,
        global_fn: impl Fn(T, &mut R),
        global_data: &mut R,
        bar: &ProgressBar,
    ) {
        match self {
            Reader::BioFasta {
                buffer,
                parallel_chunk_size,
                take_first,
            } => {
                let input_records = bio::io::fasta::Reader::from_bufread(buffer).records();

                let recs = match take_first {
                    Some(n) => itertools::Either::Right(input_records.take(n)),
                    None => itertools::Either::Left(input_records),
                };

                for bunch in &recs.chunks(parallel_chunk_size) {
                    let mut outs = Vec::new();

                    bunch
                        .collect_vec()
                        .into_par_iter()
                        .map(|result| match result {
                            Err(_) => panic!("Bad record!"),
                            Ok(record) => local_fn(Read::Fasta(FastaRead::BioFasta(&record))),
                        })
                        .collect_into_vec(&mut outs);

                    for t in outs {
                        global_fn(t, global_data)
                    }

                    bar.inc(parallel_chunk_size as u64);
                }
            }

            Reader::BioFastq {
                buffer,
                parallel_chunk_size,
                take_first,
            } => {
                let input_records = bio::io::fastq::Reader::from_bufread(buffer).records();

                let recs = match take_first {
                    Some(n) => itertools::Either::Right(input_records.take(n)),
                    None => itertools::Either::Left(input_records),
                };

                for bunch in &recs.chunks(parallel_chunk_size) {
                    let mut outs = Vec::new();

                    bunch
                        .collect_vec()
                        .into_par_iter()
                        .map(|result| match result {
                            Err(_) => panic!("Bad record!"),
                            Ok(record) => local_fn(Read::Fastq(FastqRead::BioFastq(&record))),
                        })
                        .collect_into_vec(&mut outs);

                    for t in outs {
                        global_fn(t, global_data)
                    }

                    bar.inc(parallel_chunk_size as u64);
                }
            }

            Reader::BioFastqSingleThreaded { buffer, take_first } => {
                let input_records = bio::io::fastq::Reader::from_bufread(buffer).records();

                let recs = match take_first {
                    Some(n) => itertools::Either::Right(input_records.take(n)),
                    None => itertools::Either::Left(input_records),
                };

                recs.for_each(|res| match res {
                    Ok(rec) => {
                        // perform the global and local functions!
                        global_fn(
                            local_fn(Read::Fastq(FastqRead::BioFastq(&rec))),
                            global_data,
                        );

                        bar.inc(1);
                    }
                    Err(_) => panic!("Bad record!"),
                });
            }

            Reader::BioFastaSingleThreaded { buffer, take_first } => {
                let input_records = bio::io::fasta::Reader::from_bufread(buffer).records();

                let recs = match take_first {
                    Some(n) => itertools::Either::Right(input_records.take(n)),
                    None => itertools::Either::Left(input_records),
                };

                recs.for_each(|res| match res {
                    Ok(rec) => {
                        // perform the global and local functions!
                        global_fn(
                            local_fn(Read::Fasta(FastaRead::BioFasta(&rec))),
                            global_data,
                        );

                        bar.inc(1);
                    }
                    Err(_) => panic!("Bad record!"),
                });
            }

            Reader::SeqList {
                buffer,
                parallel_chunk_size,
                take_first,
            } => {
                let input_lines = buffer.lines();

                let lines = match take_first {
                    Some(n) => itertools::Either::Right(input_lines.take(n)),
                    None => itertools::Either::Left(input_lines),
                };

                for bunch in &lines.chunks(parallel_chunk_size) {
                    let mut outs = Vec::new();

                    bunch
                        .collect_vec()
                        .into_par_iter()
                        .map(|result| match result {
                            Ok(line) => local_fn(Read::Seq(line.as_bytes())),
                            Err(_) => panic!("Bad record!"),
                        })
                        .collect_into_vec(&mut outs);

                    for out in outs {
                        global_fn(out, global_data);
                    }

                    bar.inc(parallel_chunk_size as u64);
                }
            }
            Reader::NoodlesSam {
                buffer,
                parallel_chunk_size,
                take_first,
            } => {
                let mut reader = noodles::sam::io::Reader::new(buffer);

                let header = reader.read_header();

                let input_records = reader.records();

                let recs = match take_first {
                    Some(n) => itertools::Either::Right(input_records.take(n)),
                    None => itertools::Either::Left(input_records),
                };

                for bunch in &recs.chunks(parallel_chunk_size) {
                    let mut outs = Vec::new();

                    bunch
                        .collect_vec()
                        .into_par_iter()
                        .map(|result| match result {
                            Ok(rec) => local_fn(Read::Sam(SamRead::Noodles {
                                record: &rec,
                                cigar: &rec.cigar(),
                                seq: &rec.sequence(),
                                qual: &rec.quality_scores(),
                                data: &rec.data(),
                            })),
                            Err(_) => panic!("Bad record!"),
                        })
                        .collect_into_vec(&mut outs);

                    for out in outs {
                        global_fn(out, global_data);
                    }

                    bar.inc(parallel_chunk_size as u64);
                }
            }
            Reader::BioPairedEndFastq {
                buffer1,
                buffer2,
                parallel_chunk_size,
                take_first,
            } => {
                let input_records1 = bio::io::fastq::Reader::from_bufread(buffer1).records();
                let input_records2 = bio::io::fastq::Reader::from_bufread(buffer2).records();
                let input_records = input_records1.zip(input_records2);

                let recs = match take_first {
                    Some(n) => itertools::Either::Right(input_records.take(n)),
                    None => itertools::Either::Left(input_records),
                };

                for bunch in &recs.chunks(parallel_chunk_size) {
                    let mut outs = Vec::new();

                    bunch
                        .collect_vec()
                        .into_par_iter()
                        .map(|result| match result {
                            (Ok(record1), Ok(record2)) => local_fn(Read::PairedEnd {
                                r1: &Read::Fastq(FastqRead::BioFastq(&record1)),
                                r2: &Read::Fastq(FastqRead::BioFastq(&record2)),
                            }),
                            _ => panic!("Bad record!"),
                        })
                        .collect_into_vec(&mut outs);

                    for t in outs {
                        global_fn(t, global_data)
                    }

                    bar.inc(parallel_chunk_size as u64);
                }
            }
            Reader::BioPairedEndFasta {
                buffer1,
                buffer2,
                parallel_chunk_size,
                take_first,
            } => {
                let input_records1 = bio::io::fasta::Reader::from_bufread(buffer1).records();
                let input_records2 = bio::io::fasta::Reader::from_bufread(buffer2).records();
                let input_records = input_records1.zip(input_records2);

                let recs = match take_first {
                    Some(n) => itertools::Either::Right(input_records.take(n)),
                    None => itertools::Either::Left(input_records),
                };

                for bunch in &recs.chunks(parallel_chunk_size) {
                    let mut outs = Vec::new();

                    bunch
                        .collect_vec()
                        .into_par_iter()
                        .map(|result| match result {
                            (Ok(record1), Ok(record2)) => local_fn(Read::PairedEnd {
                                r1: &Read::Fasta(FastaRead::BioFasta(&record1)),
                                r2: &Read::Fasta(FastaRead::BioFasta(&record2)),
                            }),
                            _ => panic!("Bad record!"),
                        })
                        .collect_into_vec(&mut outs);

                    for t in outs {
                        global_fn(t, global_data)
                    }

                    bar.inc(parallel_chunk_size as u64);
                }
            }
        };
    }
}

use itertools::Itertools;
use std::{
    fs::File,
    io::{stdin, BufReader},
    path::Path,
};

#[derive(Debug)]
pub enum FileTypeError {
    UnknownFileType,
    UnsupportedPairedCombination(String),
}

pub enum FileType {
    Fasta,
    Fastq,
    Sam,
    Bam,
    List,
}

impl TryFrom<&str> for FileType {
    type Error = FileTypeError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "fa" => Ok(FileType::Fasta),
            "fasta" => Ok(FileType::Fasta),

            "fq" => Ok(FileType::Fastq),
            "fastq" => Ok(FileType::Fastq),

            "list" => Ok(FileType::List),
            "txt" => Ok(FileType::List),

            "sam" => Ok(FileType::Sam),
            "bam" => Ok(FileType::Bam),

            _ => Err(FileTypeError::UnknownFileType),
        }
    }
}

#[derive(Debug)]
pub enum InputError {
    FileNameError,
    FileTypeError(FileTypeError),
    FileOpenError(std::io::Error),
}

/// Checks the arguments, and either opens a file or reads from stdin.
/// Also returns the filetype.
fn reader(input_file: &str) -> Result<(FileType, Box<dyn BufRead>), InputError> {
    if input_file.eq("stdin") {
        // just read straight from stdin
        Ok((FileType::Fasta, Box::new(BufReader::new(stdin()))))
    } else {
        let path = Path::new(&input_file);

        let file = File::open(path).map_err(InputError::FileOpenError)?;

        match &input_file.split('.').collect_vec()[..] {
            [.., ext, "gz"] => match FileType::try_from(*ext) {
                Ok(filetype) => Ok((
                    filetype,
                    Box::new(BufReader::new(flate2::read::MultiGzDecoder::new(file))),
                )),
                Err(err) => Err(InputError::FileTypeError(err)),
            },

            [.., ext] => match FileType::try_from(*ext) {
                Ok(filetype) => Ok((filetype, Box::new(BufReader::new(file)))),
                Err(err) => Err(InputError::FileTypeError(err)),
            },

            _ => Err(InputError::FileNameError),
        }
    }
}
