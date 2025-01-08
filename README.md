# Matchbox

Matchbox is a flexible read processor, capable of performing powerful transformations on your FASTA/FASTQ/SAM files.

*This is an alpha version, and I guarantee I will make breaking changes to matchbox syntax and behaviour. The final version will feel either a little bit different, or completely different. Use at your own risk! Italicised annotations on this readme explain features I'm looking to redesign. If you have any ideas about any of this, please share them with me!*

# Installation

Currently, you need to clone this repo and build it with cargo.

```
git clone ..
cd matchbox-alpha
cargo build --release
./target/release/matchbox-alpha --help
```

# Usage

A matchbox script can be executed across your reads with the `run` subcommand.

```bash
matchbox run "read.id" reads.fq
```

When you want to write longer matchbox code, put it in a `.mb` file and use the `script` subcommand.

```bash
matchbox script "trim.mb" reads.fq
```

<!-- # Example scripts

Trim off standard PCB109 primers,  -->

# Scripting language reference

Matchbox doesn't do anything on its own - it requires a script to tell it what to do with each read in your input, written in the matchbox scripting language.

Matchbox scripts are made of statements, of which there are a few different types.

## Send statements

A send statement has an expression on the left to evaluate, and an output location on the right, to which the resulting value will be sent. The expression and the output location are separated by a pipe `|>`.

**Example:** Send the name of each read to an output file called `names.txt`.

```python
read.id |> file('ids.txt')
```

You can omit the output location, which will send the value to stdout.

**Example:** Print the full sequence of each read to stdout.
```python
read.seq
```

A list of output locations is [given below](#output-locations).

## Assignment with `:`

Using a colon `:`, you can assign a name to a value, so that it can be used as shorthand elsewhere in the script. Names can contain letters, numbers and underscores (though they cannot start with a number).

**Example:** Print out the ID of each read to a file called `ids.txt`.
```python
filename: 'ids.txt'
output: file(filename)

read.id |> output
```

## Conditional statements with `if`

Statements can be conditional using `if`, only executing the branch on the right of the arrow `=>` when their conditions are met.

**Example:** If a read is longer than 1000 bases, send it to a file `long.fq`.

```python
if read.seq.len() > 1000 => read |> file('long.fq')
```

### Pattern matching

Conditional statements can include pattern-matching, allowing reads to be taken apart structurally. The pattern, enclosed in square brackets `[]`, describes a geometry which must match the read for the statement on the right of the arrow `=>` to execute. Fixed sequences can be used in patterns (or variables which refer to fixed sequences, such as `primer` in the below example). Additionally, variable regions denoted with underscores `_` can be used, each consisting of any number of nucleotides.

**Example:** If a read contains a primer (with any number of nucleotides on either side), send it to a file `filtered.fq`.
```python
primer: AGTCAGTGCTAG

if read is [_ primer _] => read |> file('filtered.fq')
```

When a pattern matches a read multiple times (e.g. when multiple primers are present), only the first match of the pattern is executed. *I'm working on optionally allowing you to execute the branch each time the pattern is matched within each read.*

Edit distance is allowed when matching, and the command-line argument `error-rate` allows the user to decide what proportion of each sequence needs to match. *I'm working on a feature to allow you to specify error rate for each individual sequence, if you wish to.*

When there are multiple patterns to be matched, they are tried in sequence, and the first successful branch is executed.

**Example:** If a read contains a primer, send it to a file `forward.fq`. Otherwise, if it contains the reverse complement of the primer, send it to `reverse.fq`.
```python
primer: AGTCAGTGCTAG

if read is
    [_ primer _] => read |> file('forward.fq')
    [_ -primer _] => read |> file('reverse.fq')
```

### Extracting regions with `:`

Often, you want to extract specific parts of a read, while trimming off other parts. Using a colon `:`, you can bind a user-defined name to a slice of the read. This slice retains the metadata of the read but includes only the part of the sequence that falls within the specified region; hence, it's the simplest way to trim reads in matchbox.

**Example:** If a read contains `AGAG` and later `CTCT`, then call the region in between them `mid`, and then print out this trimmed region `mid` to a file `filtered.fa`.
```python
if read is [_ AGAG mid:_ CTCT _] => mid |> file('filtered.fa')
```

For FASTQ and SAM, the quality score is also trimmed to only contain the variable region. *Other SAM fields like the modification string are not yet correctly handled by this slicing. Bugs galore*

### Fixed-length regions with `|n|`

To specify a region of a fixed length `n`, vertical bars `|n|` can be used.

**Example:** If a read contains the sequence `AGAG` and later `CTCT`, then extract four bases following `AGAG`, and call these `bc`. Extract everything after this until `CTCT`, and call it `mid`. Then, tag this trimmed region `mid` with the barcode sequence that was found, and print the trimmed reads to a file `filtered.fa`.
```python
if read is [_ AGAG bc:|4| mid:_ CTCT _] =>
    mid.tag('barcode={bc.seq}') |> file('filtered.fa')
```

**Example:** Take the first four bases of each read, and accumulate them into counts. This will report how frequently a read starts with each possible set of four bases.
```python
if read is [first:|4| _] => first.seq |> counts
```

### Parameterising patterns with `for`

Sometimes, a read contains a sequence from a known list of sequences (e.g. a barcode list). With the `for` syntax, you can introduce a named parameter to handle such regions. For each value from the list that can be used match the pattern, the branch is executed. *I'm adding a feature to specify whether you want to execute the branch for all matching barcodes, or just the best matching one.*

**Example:** Load a list of barcodes from `barcodes.tsv`. If a read contains a primer immediately followed by a barcode sequence, send the read to `filtered.fq`. Also, send the name of the matched barcode to `counts`, to quantify which barcodes appeared most frequently. If a read did not contain the primer and barcode structure, send it to `bad.fq`. (For this script to work, your barcode TSV must contain columns called 'name' and 'seq'.)

```python
primer: AGTCAGTGCTAG
bcs: tsv('barcodes.tsv')

'total' |> counts

if read is
    [_ primer bc.seq _] for bc in bcs => {
        bc.name |> counts
        read |> file('filtered.fq')
    }
    [_] => read |> file('bad.fq')
```

## Grouping `{}` and separation `;`

As shown above, curly braces `{}` can be used to group several statements together so that they are all executed. Statements are separated either by newlines, or by semicolons `;`.

**Example:** For reads over 1000 bases in length, collect them in a file `filtered.fq`, and print their names to stdout.
```python
if read.seq.len() > 1000 => { read.id; read |> file('filtered.fq') }
```

## Descriptive analysis with `as`

Pattern matching is great when you're already sure of what structure your reads have. First, though, it's good to search through your reads to discover what structure is present. An `as` expression allows you to infer a read's structure in terms of some fixed sequences you expect to find.

**Example:** Describe a read in terms of the presence of TSO sequences and poly(A) tails. Accumulate these into counts, to get a summary of general read structure.
```python
tso: AGCTGATCG
read as { poly_a: AAAAAAAA, tso } |> counts
```

## Outputs

There are a few built-in output locations to which you can send values. They accumulate output from across all of your reads, and they are the way of getting filtered or summarised information out of matchbox.

*These are a weird part of matchbox, and I'm thinking about how to redesign them. E.g. you should be able to send values into counts and then send the counts output into a file, but you can't chain outputs in this way. Also, you should be able to count different types of things - e.g. you have a read structure with two barcodes, and want to count occurrences of each barcode type seperately - but you can't, because counts is a single global output. Clearly there are limitations to this model.*

<table>
    <tr>
        <th>Name</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><code>stdout</code></td>
        <td>Values are printed directly to stdout.</td>
    </tr>
    <tr>
        <td><code>file(exp)</code></td>
        <td>Values are printed to a file whose name is given by exp. If the filename indicates a FASTA, FASTQ or SAM/BAM file, the values will be formatted as appropriate. If the values can't be treated as FASTA/FASTQ/SAM reads, matchbox will crash.</td>
    </tr>
    <tr>
        <td><code>counts</code></td>
        <td>Values are counted and, after processing your reads, the number of times each unique value was sent to <code>counts</code> is printed to stdout.</td>
    </tr>
    <tr>
        <td><code>average</code></td>
        <td>Values must be numeric. After processing your reads, the average and standard deviation of all accumulated values is printed to stdout.</td>
    </tr>
</table>

## Expressions

### Variables

An expression can be a variable name, assuming that name has been bound in this scope, or in any looser scope.

#### Input read formats

`read` is a built-in variable which refers to the current read being processed. It has fields that can be accessed using dot `.` notation.

FASTA, FASTQ and SAM reads contain slightly different fields. Depending on the file type you are inputting into matchbox, different fields will be available to you.

<table>
    <tr>
        <th>Field</th>
        <th>Type</th>
        <th>Description</th>
        <th>FASTA</th>
        <th>FASTQ</th>
        <th>SAM</th>
    </tr>
    <tr>
        <td><code>seq</code></td>
        <td><b>Str</b></td>
        <td>The raw nucleotide sequence of the read.</td>
        <td>✅</td>
        <td>✅</td>
        <td>✅</td>
    </tr>
    <tr>
        <td><code>desc</code></td>
        <td><b>Str</b></td>
        <td>The entire description line of the read, after the ID. For FASTA, sometimes referred to as the definition line. </td>
        <td>✅</td>
        <td>✅</td>
        <td></td>
    </tr>
    <tr>
        <td><code>id</code></td>
        <td><b>Str</b></td>
        <td>The name of the read. For FASTA, this is anything from the <code>></code> to the first whitespace in the description line. For FASTQ, this is anything from the <code>@</code> to the first whitespace. For SAM, this is the <code>qname</code> column.</td>
        <td>✅</td>
        <td>✅</td>
        <td>✅</td>
    </tr>
    <tr>
        <td><code>qual</code></td>
        <td><b>Str</b></td>
        <td>The quality score of the read, as a string of ASCII-encoded Phred scores. Found in the fourth line of a FASTQ read, or the <code>qual</code> field of a SAM.</td>
        <td></td>
        <td>✅</td>
        <td>✅</td>
    </tr>
    <tr>
        <td><code>flag</code></td>
        <td><b>Num</b></td>
        <td>The flag, which encodes a lot of metadata about the read.</td>
        <td></td>
        <td></td>
        <td>✅</td>
    </tr>
    <tr>
        <td><code>rname</code></td>
        <td><b>Str</b></td>
        <td>The reference name.</td>
        <td></td>
        <td></td>
        <td>✅</td>
    </tr>
    <tr>
        <td><code>mapq</code></td>
        <td><b>Num</b></td>
        <td>The quality of the mapping, if appropriate. -1 otherwise.</td>
        <td></td>
        <td></td>
        <td>✅</td>
    </tr>
    <tr>
        <td><code>rnext</code></td>
        <td><b>Num</b></td>
        <td>The next read, if applicable.</td>
        <td></td>
        <td></td>
        <td>✅</td>
    </tr>
    <tr>
        <td><code>pnext</code></td>
        <td><b>Num</b></td>
        <td>The starting position of the next read.</td>
        <td></td>
        <td></td>
        <td>✅</td>
    </tr>
    <tr>
        <td><code>tlen</code></td>
        <td><b>Num</b></td>
        <td>The template length.</td>
        <td></td>
        <td></td>
        <td>✅</td>
    </tr>
    <!-- <tr>
        <td><code>tags</code></td>
        <td><b>Rec</b></td>
        <td>The tags associated with the read. For FASTA, this is a list of space-separated, <code>key=value</code> pairs after the name in the description line. For FASTQ, . For SAM, a list of </td>
        <td>✅</td>
        <td>✅</td>
        <td>✅</td>
    </tr> -->
</table>

### Functions

A function call is another type of expression. Matchbox's small set of built-in functions allow you to transform data.

To call a function in matchbox, the syntax is similar to most other languages: the function's name, followed by parentheses which contain a comma-separated list of arguments.

```python
x: len(read.seq)

slice(read, 0, x / 2) |> file('halved_reads.fq')
```

To allow for programming in an OOP method-chaining style, functions can be called as though they are methods of their first argument (i.e. their first argument is moved before the dot):

```python
x: read.seq.len()

read.slice(0, x / 2) |> file('halved_reads.fq')
```

The full function list is given here:

<table>
    <tr>
        <th>Name</th>
        <th>Argument types</th>
        <th>Return type</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><code>len</code> / <code>length</code></td>
        <td><b>Str</b> / <b>Seq</b> / <b>List</b></td>
        <td><b>Num</b></td>
        <td>Find the length of a string, sequence or list.</td>
    </tr>
    <tr>
        <td><code>rename</code></td>
        <td><b>Rec</b>,<br><b>Str</b></td>
        <td><b>Rec</b></td>
        <td>Create a new record, with an updated 'name' field.</td>
    </tr>
    <tr>
        <td><code>slice</code></td>
        <td><b>Rec</b> / <b>Seq</b> / <b>Str</b>,<br><b>Num</b>,<br><b>Num</b></td>
        <td><b>Rec</b> / <b>Seq</b> / <b>Str</b></td>
        <td>Take a slice of a read, sequence or string, from a starting index to an ending index. Note that slicing is inclusive of the start and exclusive of the end, so <code>slice(read, n, m)</code> is the interval <code>n <= x < m</code>. Hence, you can say <code>slice(read, 0, len(read.seq))</code>.</td>
    </tr>
    <tr>
        <td><code>translate</code></td>
        <td><b>Seq</b></td>
        <td><b>Str</b></td>
        <td>Convert a sequence to its amino acid string.</td>
    </tr>
    <tr>
        <td><code>concat</code></td>
        <td><b>Rec</b>,<br><b>Rec</b></td>
        <td><b>Rec</b></td>
        <td>Concatenate two reads (or slices of a read) together. The metadata of the first read is kept (ID, description, etc.) while the sequences (and quality scores, if applicable) of the two reads are concatenated.</td>
    </tr>
    <tr>
        <td><code>to_str</code></td>
        <td><b>Bool</b> / <b>Num</b> / <b>Seq</b></td>
        <td><b>Str</b></td>
        <td>Convert a value to string format.</td>
    </tr>
    <tr>
        <td><code>to_seq</code></td>
        <td><b>Str</b></td>
        <td><b>Seq</b></td>
        <td>Convert a value to a sequence.</td>
    </tr>
    <tr>
        <td><code>csv</code></td>
        <td><b>Str</b></td>
        <td><b>List(Rec)</b></td>
        <td>Given a filename, open the file as CSV and read all the rows into a list of records. Each record has fields named after the columns of the CSV, with values from the given row.</td>
    </tr>
    <tr>
        <td><code>tsv</code></td>
        <td><b>Str</b></td>
        <td><b>List(Rec)</b></td>
        <td>Given a filename, open the file as TSV and read all the rows into a list of records. Each record has fields named after the columns of the TSV, with values from the given row.</td>
    </tr>
</table>

*todo: write up the rest of the functions*

### Operators

Operators are the same as functions, but are often infix

<table>
    <tr>
        <th>Operator</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><b>Num</b> <code>+</code> <b>Num</b></td>
        <td>Addition</td>
    </tr>
    <tr>
        <td><b>Num</b> <code>-</code> <b>Num</b></td>
        <td>Subtraction</td>
    </tr>
    <tr>
        <td><b>Num</b> <code>*</code> <b>Num</b></td>
        <td>Multiplication</td>
    </tr>
    <tr>
        <td><b>Num</b> <code>/</code> <b>Num</b></td>
        <td>Division</td>
    </tr>
    <tr>
        <td><code>-</code> <b>Num</b></td>
        <td>Negation</td>
    </tr>
    <tr>
        <td><code>-</code> (<b>Seq</b> / <b>Rec</b>)</td>
        <td>Reverse complementation (of a sequence or a read)</td>
    </tr>
    <tr>
        <td><b>Num</b> <code>%</code> <b>Num</b></td>
        <td>Modulo</td>
    </tr>
    <tr>
        <td><b>Any</b> <code>|></code> <b>Output</b></td>
        <td>Send a value to the output</td>
    </tr>
</table>

## Todo

Features I'm currently adding to matchbox.

- Support for paired-end reads
- Ability to access input file name in the script (so you can call your outputs like `'{input_file_name}_trimmed.fq'`)
- Handle SAM optional fields
- Ability to combine strings/seqs with binary `+` operator. so `read.tag('bc={bc1.seq+bc2.seq+bc3.seq}')` works intuitively
  - Maybe make `tag` take many args?
- Concatenate two parts of a read together with an operator, e.g. when rotating a plasmid `[part1:_ part2:(AGCTAGTCG _)] => part2 + part1 |> file('rotated.fq')`
- Figure out the file size estimation problem on gzipped files
- Make sequence values case insensitive
  - Make sequences permit IUPAC codes
