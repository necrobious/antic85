// the RFC1924 alphabet, originally an april 1 RFC joke, but favorable when working with JSON,
// as encoded data can be added to a JSON collection without the need to have the encoded data
// further encoded, as is required of other base85 varients.

use std::error;
use std::fmt;


//----ALPHABET-INDEX-(hex)--------0---0---0---0---1---1---1---1---2---2---2---2---3---3---3---3---4---4---4---4---5---5
//--------------------------------0---4---8---C---0---4---8---C---0---4---8---C---0---4---8---C---0---4---8---C---0---4
//--------------------------------|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
//----ALPHABET-INDEX-(dec)--------|---|---|---1---1---2---2---2---3---3---4---4---4---5---5---6---6---6---7---7---8---8
//--------------------------------0---4---8---2---6---0---4---8---2---6---0---4---8---2---6---0---4---8---2---6---0---4
const ALPHABET: &'static [u8] = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$%&()*+-;<=>?@^_`{|}~";
//--------------------------------|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
//----ASCII-(dec)-----------------4455555555666667777777777888888888899991111111111111111111111133333444445666669991111
//--------------------------------8901234567567890123456789012345678907890000000000111111111122235678012359012344562222
//--------------------------------|---|---|---|---|---|---|---|---|---|--01234567890123456789012-------------------3456
//--------------------------------|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
//----ASCII-(hex)-----------------3333333333444444444444444555555555556666666666666667777777777722222222223333345567777
//--------------------------------0123456789123456789ABCDEF0123456789A123456789ABCDEF0123456789A1345689ABDBCDEF0EF0BCDE

// the ALPHABET_INDEX represents the 94 chars that range from 33-126 as a lookup of an ascii char in the ALPHABET, 
// using (it's ascii char value - 33) as the index here in the ALPHABET_INDEX array.
// 33 is the first ascii value in the ALPHABET (the '!' char).
// Ascii chars between 33-126 that are not in the RFC1924 alphabet are represented with the value -0x01
const ALPHABET_INDEX: &'static [i8] = &[
// ALPHABET location | ALPHABET_INDEX location | ascii
     0x3e            , //      1               |  33 !
    -0x01            , //      2               |  34
     0x3f            , //      3               |  35 #
     0x40            , //      4               |  36 $
     0x41            , //      5               |  37 %
     0x42            , //      6               |  38 &
    -0x01            , //      7               |  39
     0x43            , //      8               |  40 (
     0x44            , //      9               |  41 )
     0x45            , //     10               |  42 *
     0x46            , //     11               |  43 +
    -0x01            , //     12               |  44
     0x47            , //     13               |  45 -
    -0x01            , //     14               |  46
    -0x01            , //     15               |  47
     0x00            , //     16               |  48 0
     0x01            , //     17               |  49 1
     0x02            , //     18               |  50 2
     0x03            , //     19               |  51 3
     0x04            , //     20               |  52 4
     0x05            , //     21               |  53 5
     0x06            , //     22               |  54 6
     0x07            , //     23               |  55 7
     0x08            , //     24               |  56 8
     0x09            , //     25               |  57 9
    -0x01            , //     26               |  58
     0x48            , //     27               |  59 ;
     0x49            , //     28               |  60 <
     0x4a            , //     29               |  61 =
     0x4b            , //     30               |  62 >
     0x4c            , //     31               |  63 ?
     0x4d            , //     32               |  64 @
     0x0a            , //     33               |  65 A
     0x0b            , //     34               |  66 B
     0x0c            , //     35               |  67 C
     0x0d            , //     36               |  68 D
     0x0e            , //     37               |  69 E
     0x0f            , //     38               |  70 F
     0x10            , //     39               |  71 G
     0x11            , //     40               |  72 H
     0x12            , //     41               |  73 I
     0x13            , //     42               |  74 J
     0x14            , //     43               |  75 K
     0x15            , //     44               |  76 L
     0x16            , //     45               |  77 M
     0x17            , //     46               |  78 N
     0x18            , //     47               |  79 O
     0x19            , //     48               |  80 P
     0x1a            , //     49               |  81 Q
     0x1b            , //     50               |  82 R
     0x1c            , //     51               |  83 S
     0x1d            , //     52               |  84 T
     0x1e            , //     53               |  85 U
     0x1f            , //     54               |  86 V
     0x20            , //     55               |  87 W
     0x21            , //     56               |  88 X
     0x22            , //     57               |  89 Y
     0x23            , //     58               |  90 Z
    -0x01            , //     59               |  91
    -0x01            , //     60               |  92
    -0x01            , //     61               |  93
     0x4e            , //     62               |  94 ^
     0x4f            , //     63               |  95 _
     0x50            , //     64               |  96 `
     0x24            , //     65               |  97 a
     0x25            , //     66               |  98 b
     0x26            , //     67               |  99 c
     0x27            , //     68               | 100 d
     0x28            , //     69               | 101 e
     0x29            , //     70               | 102 f
     0x2a            , //     71               | 103 g
     0x2b            , //     72               | 104 h
     0x2c            , //     73               | 105 i
     0x2d            , //     74               | 106 j
     0x2e            , //     75               | 107 k
     0x2f            , //     76               | 108 l
     0x30            , //     77               | 109 m
     0x31            , //     78               | 110 n
     0x32            , //     79               | 111 o
     0x33            , //     80               | 112 p
     0x34            , //     81               | 113 q
     0x35            , //     82               | 114 r
     0x36            , //     83               | 115 s
     0x37            , //     84               | 116 t
     0x38            , //     85               | 117 u
     0x39            , //     86               | 118 v
     0x3a            , //     87               | 119 w
     0x3b            , //     88               | 120 x
     0x3c            , //     89               | 121 y
     0x3d            , //     90               | 122 z
     0x51            , //     91               | 123 {
     0x52            , //     92               | 124 |
     0x53            , //     93               | 125 }
     0x54            , //     94               | 126 ~
];

#[derive(Clone, Copy, PartialEq)]
pub enum FromBase85Error {
    InvalidByte(char, usize),
    InvalidInputLength(usize),
    InvalidOutputLength(usize),
    UnexpectedOutputLength(usize,usize),
}

impl fmt::Debug for FromBase85Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FromBase85Error::InvalidByte(ch, idx) => write!(f, "Invalid decode base85 character '{}' at position {}.", ch, idx),
            FromBase85Error::InvalidInputLength(len) => write!(f, "Invalid decode input length {}.", len),
            FromBase85Error::InvalidOutputLength(len) => write!(f, "Invalid decode output length {}.", len),
            FromBase85Error::UnexpectedOutputLength(expected, actual) =>
                write!(f, "Invalid decode output length: expected {}, received {}.", expected, actual),
        }
    }
}

impl fmt::Display for FromBase85Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self, f)
    }
}


impl error::Error for FromBase85Error {
    fn description(&self) -> &str {
        match *self {
            FromBase85Error::InvalidByte(_, _) => "invalid character",
            FromBase85Error::InvalidInputLength(_) => "invalid decode input length",
            FromBase85Error::InvalidOutputLength(_) => "invalid decode output length",
            FromBase85Error::UnexpectedOutputLength(_,_) => "unexpected decode output length",
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum ToBase85Error {
    InvalidInputLength(usize),
    InvalidOutputLength(usize),
    UnexpectedOutputLength(usize,usize),
}

impl fmt::Display for ToBase85Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self, f)
    }
}

impl error::Error for ToBase85Error {
    fn description(&self) -> &str {
        "invalid input size"
    }
}

impl fmt::Debug for ToBase85Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ToBase85Error::InvalidInputLength(len) =>
                write!(f, "Invalid encode input size {}.", len),
            ToBase85Error::InvalidOutputLength(len) =>
                write!(f, "Invalid encode output size {}.", len),
            ToBase85Error::UnexpectedOutputLength(expected, actual) =>
                write!(f, "Invalid encode output length: expected {}, received {}.", expected, actual),
        }
    }
}

pub fn decode (input: &str) -> Result<Vec<u8>, FromBase85Error> {
    use FromBase85Error::*;
    let chars = input.chars().collect::<Vec<char>>();
    let in_len = chars.len();
    let out_len = calculate_decoding_output_length(in_len);
    if out_len == 0 { return Err(InvalidInputLength(in_len)) }; 
    let mut output: Vec<u8> = Vec::with_capacity(out_len);
    let mut in_pos: usize = 0;
    let mut out_pos: usize = 0;
    let mut buffer: [u8; 4] = Default::default();
    loop {
        let in_nxt_pos = if in_pos + 5 < in_len { in_pos + 5 } else { in_len };
        let out_nxt_pos = if out_pos + 4 < out_len { out_pos + 4 } else { out_len };
        let exp_len = out_nxt_pos - out_pos;
        let bytes_written = from_base85_chunk(&chars[in_pos..in_nxt_pos], &mut buffer[0..exp_len])
            .map_err(|err| match err { InvalidByte(c, p) => InvalidByte(c, p+in_pos), e => e })?;
        if bytes_written != exp_len { return Err(UnexpectedOutputLength(exp_len, bytes_written)) }
        output.extend_from_slice(&buffer[0..exp_len]);
        if exp_len < 4 || in_nxt_pos == in_len { break }
        in_pos = in_nxt_pos;
        out_pos = out_nxt_pos;
    }
    Ok(output)
}

pub fn encode (input: &[u8]) -> Result<String, ToBase85Error>  {
    use ToBase85Error::*;
    let in_len = input.len();
    let out_len = calculate_encoding_output_length(in_len);
    let mut output: Vec<char> = Vec::with_capacity(out_len);
    let mut in_pos: usize = 0;
    let mut out_pos: usize = 0;
    let mut buffer: [char; 5] = Default::default();
    loop {
        let in_nxt_pos = if in_pos + 4 < in_len { in_pos + 4 } else { in_len };
        let out_nxt_pos = if out_pos + 5 < out_len { out_pos + 5 } else { out_len };
        let exp_len = out_nxt_pos - out_pos;
        let bytes_written = to_base85_chunk(&input[in_pos..in_nxt_pos], &mut buffer[0..exp_len])?;    
        if bytes_written != exp_len { return Err(UnexpectedOutputLength(exp_len, bytes_written)) }
        output.extend_from_slice(&buffer[0..exp_len]);
        if exp_len < 5 || in_nxt_pos == in_len { break }
        in_pos = in_nxt_pos;
        out_pos = out_nxt_pos;
    }
    Ok(output.into_iter().collect::<String>())
}

// accepts a slice of 2-5 chars and writes them decoded from base85 into bytes, returning the
// number of bytes written into the output slice.
// 2 char  -> 1 byte 
// 3 char  -> 2 byte 
// 4 char  -> 3 byte 
// 5 char  -> 4 byte 
pub fn from_base85_chunk (input: &[char], output: &mut [u8]) -> Result<usize, FromBase85Error> {
    use FromBase85Error::*;
    // ensoure we have enough input data and output space to succeed
    let in_len = input.len();
    // we expect an an input between 2 and 5 characters
    if in_len > 5 || in_len < 2 { return Err(InvalidInputLength(in_len)) }
    // we expect the output to accept between 1 and 4 bytes
    let out_len = in_len - 1;
    if output.len() < out_len { return Err(InvalidOutputLength(out_len)) }

    let mut accum: u32 = 0;
    for c in input {
        if (*c as usize) < 33 || (*c as usize) > 126 { return Err(InvalidByte(*c, 0)) }
        let index = ALPHABET_INDEX[(*c as usize) - 33];
        if index == -1 { return Err(InvalidByte(*c, 0)) }
        accum = accum * 85 + index as u32;
    }
    let mut i = out_len;
    while i > 0 {
        i -= 1;
        output[i] = (accum % 256) as u8;
        accum = accum / 256;
    }
    Ok(out_len)
}

// accepts a slice of 1-4 bytes and writes them encoded into base85 characters, returning the
// number of characters written into the output slice.
// 1 byte -> 2 char
// 2 byte -> 3 char
// 3 byte -> 4 char
// 4 byte -> 5 char
pub fn to_base85_chunk (input: &[u8], output: &mut [char]) -> Result<usize,ToBase85Error> {
    use ToBase85Error::*;
    // ensoure we have enough input data and output space to succeed
    let in_len = input.len();
    if in_len > 4 || in_len < 1 { return Err(InvalidInputLength(in_len)) }
    let out_len = in_len + 1;
    if output.len() < out_len { return Err(InvalidOutputLength(out_len)) }
    
    // pile four byes into a u32 int
    let mut accum: u32 = 0;
    for byte in input {
        accum = accum * 256 + (*byte as u32);
    }

    // divide the u32 sum of the 4 input bytes into 5 charaters
    let mut i = out_len;
    while i > 0 {
        i -= 1;
        output[i] = ALPHABET[(accum % 85) as usize] as char;
        accum = accum / 85;
    }
    Ok(out_len)
}

//-- base85 utilities

// computes the number of characters to expect, given the byte count of the input.
// This has three usecases:
//   1. when the input length is 0, the output should be 0
//   2. when the input length modulo 4 is 0, the output should be the input length / 4 * 5
//   3. when the input length modulo 4 is not 0, the output should same as usecase #2 + input % 4 + 1
#[inline]
pub fn calculate_encoding_output_length(bin_input_len: usize) -> usize {
    let modulo = bin_input_len % 4;
    (if bin_input_len < 4 { 0 } else { bin_input_len / 4 * 5 }) + match modulo { 1 => 2, 2 => 3, 3 => 4, _ => 0 }  
}

// Computes the number of bytes to expect, given the character count if the input.
// This has four usecases:
//   1. when the input length is 0 or 1, the output should be 0
//   2. when the input length modulo 5 is 0, the output should be input length / 5 * 4
//   3. when the input length modulo 5 is 1, the output should be 0 
//   4. when the input length modulo 5 is 2-4, the output should be same as usecase #2 + input % 5 - 1 
#[inline]
pub fn calculate_decoding_output_length(char_input_len: usize) -> usize {
    let modulo = char_input_len % 5;
    if modulo == 1 { return 0 }
    (if char_input_len < 5 { 0 } else { char_input_len / 5 * 4 }) + match modulo { 2 => 1, 3 => 2, 4 => 3, _ => 0 }
}


//--- Manage b85 chunks, 4 bytes == 5 chars
/*
pub fn to_b85_chunk (input: &[u8; 4]) -> [char; 5] {
    let mut accum: u32 = 0;
    for byte in input {
        accum = accum * 256 + (*byte as u32);
    }
    let temp = accum.clone();
    let mut output: [char; 5] = Default::default();
    for c in &mut output[..] {
        *c = ALPHABET[(accum % 85) as usize] as char;
        accum = accum / 85;
    }
    output.reverse();
    println!("encode of {:02x?} into {:?}; accum from 0 to {} and back to {}", input, output, temp, accum);
    output
}

pub fn from_b85_chunk (input: &[char; 5]) -> Result<[u8; 4], FromBase85Error> {
    let mut accum: u32 = 0;
    for c in input {
        if (*c as usize) < 33 || (*c as usize) > 126 { return Err(FromBase85Error::InvalidByte(*c, 0)) }
        let index = ALPHABET_INDEX[(*c as usize) - 33];
        if index == -1 { return Err(FromBase85Error::InvalidByte(*c, 0)) }
        accum = accum * 85 + index as u32;
    }
    let temp = accum.clone();
    let mut output = [0_u8; 4];
    for b in &mut output[..] {
        *b = (accum % 256) as u8;
        accum = accum / 256;
    }
    output.reverse();
    println!("decode of {:?} into {:02x?}; accum from 0 to {} and back to {}", input, output, temp, accum);
    Ok(output)
}
//---

//---  Manage veriable length inputs
pub fn to_b85 (input: &[u8]) -> String {
    let len = input.len();

    let mut padding = 0;
    let mut to_b85_output: Vec<char> = Vec::with_capacity(1 + len / 4 * 5);
    to_b85_output.push('0'); // padding value placeholder, prepended to the head of the encoded string, 
                             // replaced with the actual value at the end of encoding 
    for c in input.chunks(4) {
        let (cs, pad) = match c.len() {
            1 => (to_b85_chunk(&[c[0], 0x00, 0x00, 0x00]), 3),
            2 => (to_b85_chunk(&[c[0], c[1], 0x00, 0x00]), 2),
            3 => (to_b85_chunk(&[c[0], c[1], c[2], 0x00]), 1),
            _ => (to_b85_chunk(&[c[0], c[1], c[2], c[3]]), 0),
        };
        to_b85_output.extend_from_slice(&cs);
        padding = pad;
    }

    if to_b85_output.len() > 0 && padding > 0 {
        to_b85_output[0] = char::from(48 + padding); // update with the actuall padding count
    }

    to_b85_output.into_iter().collect::<String>()
}

pub fn from_b85 (input: &str) -> Result<Vec<u8>, FromBase85Error> {
    let len = input.len();

    if len == 0 || len % 5 != 1 { return Err(FromBase85Error::InvalidInputLength(len)) }

    let mut ch_iter = input.chars().into_iter();

    let pad = ch_iter
        .next()
        .ok_or(FromBase85Error::InvalidInputLength(0))
        .and_then(|c| c.to_digit(10).ok_or(FromBase85Error::InvalidByte(c, 0)))
        .and_then(|d| if d <= 3 { Ok(d) } else { Err(FromBase85Error::InvalidByte(std::char::from_u32(d).unwrap(), 0))})
        .and_then(|d| usize::try_from(d).map_err(|_| FromBase85Error::InvalidByte(std::char::from_u32(d).unwrap(), 0)))?;

    let mut output: Vec<u8> = Vec::with_capacity( (len / 5) * 4);
    let mut pos: usize = 0; 
    for ch in ch_iter.collect::<Vec<char>>().chunks(5) {
        let last = pos + 5 < len;
        let b = from_b85_chunk(&[ch[0], ch[1], ch[2], ch[3], ch[4]])
            .map_err(|err| match err {
                FromBase85Error::InvalidByte(c, p) => FromBase85Error::InvalidByte(c, p+pos),
                e => e,
            })?;
        output.extend_from_slice(if last { &b[0..(4 - pad)] } else { &b }); 
        pos = pos + 5;
    }
    Ok(output)
}
//---
*/


#[cfg(test)]
mod tests {
    use super::{ decode, encode, calculate_decoding_output_length, calculate_encoding_output_length, to_base85_chunk, from_base85_chunk };
    use rand; 

    #[test]
    fn variable_round_trip () {
        for i in 0..1000 {
            let rnd_size = rand::random::<usize>();
            println!("attempt {:003?}: rnd_size: {};",i, rnd_size);
            let rnd_capacity: usize = rnd_size % 100 + 1;
            println!("attempt {:003?}: rnd_capacity: {};",i, rnd_capacity);
            let mut rnd_bytes:Vec<u8> = Vec::with_capacity(rnd_capacity);
            for _ in 0..rnd_bytes.capacity() { rnd_bytes.push(rand::random::<u8>()); }
            println!("attempt {:003?}: rnd_bytes: {:02x?};",i, rnd_bytes);
            let enc_output = encode(&rnd_bytes);
            assert!(enc_output.is_ok());
            println!("attempt {:003?}: encoded output: {:02x?};",i, &enc_output);
            let dec_output = decode(&enc_output.unwrap());
            assert!(dec_output.is_ok());
            println!("attempt {:003?}: decoded output: {:02x?};",i, &dec_output);
            assert_eq!(rnd_bytes, dec_output.unwrap());
        }
    }


    #[test]
    fn calculate_encoding_output_lengths () {
        assert_eq!(calculate_encoding_output_length(0), 0); // usecase #1
        assert_eq!(calculate_encoding_output_length(1), 2); // usecase #3
        assert_eq!(calculate_encoding_output_length(2), 3); // usecase #3
        assert_eq!(calculate_encoding_output_length(3), 4); // usecase #3
        assert_eq!(calculate_encoding_output_length(4), 5); // usecase #2
        assert_eq!(calculate_encoding_output_length(5), 7); // usecase #3
        assert_eq!(calculate_encoding_output_length(6), 8); // usecase #3
        assert_eq!(calculate_encoding_output_length(7), 9); // usecase #3
        assert_eq!(calculate_encoding_output_length(8), 10);// usecase #2
    }

    #[test]
    fn calculate_decoding_output_lengths () {
        assert_eq!(calculate_decoding_output_length(0),  0); // usecase #1
        assert_eq!(calculate_decoding_output_length(1),  0); // usecase #3
        assert_eq!(calculate_decoding_output_length(2),  1); // usecase #4
        assert_eq!(calculate_decoding_output_length(3),  2); // usecase #4
        assert_eq!(calculate_decoding_output_length(4),  3); // usecase #4
        assert_eq!(calculate_decoding_output_length(5),  4); // usecase #2
        assert_eq!(calculate_decoding_output_length(6),  0); // usecase #3
        assert_eq!(calculate_decoding_output_length(7),  5); // usecase #4
        assert_eq!(calculate_decoding_output_length(8),  6); // usecase #4
        assert_eq!(calculate_decoding_output_length(9),  7); // usecase #4
        assert_eq!(calculate_decoding_output_length(10), 8); // usecase #2
        assert_eq!(calculate_decoding_output_length(11), 0); // usecase #3
    }
//    #[test]
//    fn decode_no_padding1() {
//        let t1 = "0Hello";
//        let r1 = from_b85(t1);
//        assert_eq!(r1, Ok(vec!(0x36, 0x60, 0xE3, 0x0D)));
//        let t2 = "0World";
//        let r2 = from_b85(t2);
//        assert_eq!(r2, Ok(vec!(0x65, 0x6b, 0x07, 0xf9)));
//    }


//    #[test]
//    fn decode_no_padding() {
//        let t1 = "0hELLO";
//        let r1 = from_b85(t1);
//        assert_eq!(r1, Ok(vec!(0x86, 0x4F, 0xD2, 0x6F)));
//        let t2 = "0wORLD";
//        let r2 = from_b85(t2);
//        assert_eq!(r2, Ok(vec!(0xB5, 0x59, 0xF7, 0x5B)));
//    }

//    #[test]
//    fn decode_chunk() {
//        let t1 = &['h','E','L','L','O'];
//        let r1 = from_b85_chunk(t1);
//        assert_eq!(r1, Ok([0x86, 0x4F, 0xD2, 0x6F]));
//        let t2 = &['w','O','R','L','D'];
//        let r2 = from_b85_chunk(t2);
//        assert_eq!(r2, Ok([0xB5, 0x59, 0xF7, 0x5B]));
//    }

//    #[test]
//    fn encode_no_padding() {
//        let t1 = &[0x86, 0x4F, 0xD2, 0x6F];
//        let r1 = to_b85(t1);
//        assert_eq!(r1, "0hELLO".to_string());
//        let t2 = &[0xB5, 0x59, 0xF7, 0x5B];
//        let r2 = to_b85(t2);
//        assert_eq!(r2, "0wORLD".to_string());
//    }

//    #[test]
//    fn encode_chunk() {
//        let t1 = &[0x86, 0x4F, 0xD2, 0x6F];
//        let r1 = to_b85_chunk(t1);
//        assert_eq!(&r1.iter().collect::<String>(), "hELLO");
//        let t2 = &[0xB5, 0x59, 0xF7, 0x5B];
//        let r2 = to_b85_chunk(t2);
//        assert_eq!(&r2.iter().collect::<String>(), "wORLD");
//    }

//    #[test]
//    fn encode_chunk_pads() {
//        let t1 = &[0x86, 0x00, 0x00, 0x00];
//        let r1 = to_b85_chunk(t1);
//        let t2 = &[0x86];
//        let mut o2: [char; 5] = Default::default();
//        let r2 = to_base85_chunk(t2, &mut o2);    
//        assert_eq!(r2, Ok(2));
//        let r2s = r2.unwrap();
//        let r2a = &o2[0..r2s];
//        assert_eq!(r2a.iter().collect::<String>(), "wORLD");    
//        assert_eq!(&r1.iter().collect::<String>(), "hELLO");
//    }

//    #[test]
//    fn ensure_output_char_count_is_one_plus_input_byte_count() {
//        let mut o: [char; 5] = Default::default();
//        let mut i = [0_u8; 4];
//        for idx0 in 0..256 {
//            // input of 1 byte should always result in an output of two chars
//            i[0] = idx0 as u8;
//            let r0 = to_base85_chunk(&i[0..1], &mut o);    
//            assert_eq!(r0, Ok(2), "testing byte at 0:  input: {:?}; ourput: {:?}", &i, &o);
//            for idx1 in 0..256 {
//                // input of 2 bytes should always result in an output of three chars
//                i[1] = idx1 as u8;
//                let r1 = to_base85_chunk(&i[0..2], &mut o);    
//                assert_eq!(r1, Ok(3), "testing byte at 1:  input: {:?}; ourput: {:?}", &i, &o);
//                for idx2 in 0..256 {
//                    // input of 2 bytes should always result in an output of three chars
//                    i[2] = idx2 as u8;
//                    let r2 = to_base85_chunk(&i[0..3], &mut o);    
//                    assert_eq!(r2, Ok(4), "testing byte at 2:  input: {:?}; ourput: {:?}", &i, &o);
//                }
//            }
//        }
//    }


//    #[test]
//    fn random_100_round_trips () {
//        for _ in 0..100 {
//            let mut rnd_bytes = [0_u8; 100];
//            for rnd_byte in &mut rnd_bytes[..]  {
//                *rnd_byte = rand::random::<u8>();
//            }
//    
//            let mut to_b85_output: Vec<char> = Vec::with_capacity(rnd_bytes.len() / 4 * 5);
//            for c in rnd_bytes.chunks(4) {
//                let cs = to_b85_chunk(&[c[0], c[1], c[2],c[3]]);
//                to_b85_output.extend_from_slice(&cs);
//            }
//            let b85_str = to_b85_output.into_iter().collect::<String>();
//
//            assert_eq!(125, b85_str.len());    
//    
//            let mut from_b85_output: Vec<u8> = Vec::with_capacity(rnd_bytes.len());
//            for c in b85_str.chars().collect::<Vec<char>>().chunks(5) {
//                let bs = from_b85_chunk(&[c[0], c[1], c[2],c[3],c[4]]);
//                assert_eq!(true, bs.is_ok());
//                from_b85_output.extend_from_slice(&bs.unwrap());
//            }
//    
//            assert_eq!(rnd_bytes.to_vec(), from_b85_output, "testing random bytes:{:02x?} into base85: {}", &rnd_bytes, &b85_str);
//        }
//    }
}
