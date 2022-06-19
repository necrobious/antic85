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
    InvalidDecodeLength(usize,usize),
}

impl fmt::Debug for FromBase85Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FromBase85Error::InvalidByte(ch, idx) => write!(f, "Invalid decode base85 character '{}' at position {}.", ch, idx),
            FromBase85Error::InvalidInputLength(len) => write!(f, "Invalid decode input length {}.", len),
            FromBase85Error::InvalidOutputLength(len) => write!(f, "Invalid decode output length {}.", len),
            FromBase85Error::InvalidDecodeLength(expected, actual) =>
                write!(f, "Invalid decode length: expected {}, received {}.", expected, actual),
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
            FromBase85Error::InvalidDecodeLength(_,_) => "unexpected decode output length",
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum ToBase85Error {
    InvalidInputLength(usize),
    InvalidOutputLength(usize),
    InvalidEncodeLength(usize,usize),
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
            ToBase85Error::InvalidEncodeLength(expected, actual) =>
                write!(f, "Invalid encode length: expected {}, received {}.", expected, actual),
        }
    }
}

pub fn decode (input: &str) -> Result<Vec<u8>, FromBase85Error> {
//pub fn decode<T: AsRef<[u8]>>(input: T) -> Result<Vec<u8>, FromBase85Error> {
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
        if bytes_written != exp_len { return Err(InvalidDecodeLength(exp_len, bytes_written)) }
        output.extend_from_slice(&buffer[0..exp_len]);
        if exp_len < 4 || in_nxt_pos == in_len { break }
        in_pos = in_nxt_pos;
        out_pos = out_nxt_pos;
    }
    Ok(output)
}

pub fn encode<T: AsRef<[u8]>>(input: T) -> Result<String, ToBase85Error>  {
    use ToBase85Error::*;
    let bytes = input.as_ref();
    let in_len = bytes.len();
    let out_len = calculate_encoding_output_length(in_len);
    let mut output: Vec<char> = Vec::with_capacity(out_len);
    let mut in_pos: usize = 0;
    let mut out_pos: usize = 0;
    let mut buffer: [char; 5] = Default::default();
    loop {
        let in_nxt_pos = if in_pos + 4 < in_len { in_pos + 4 } else { in_len };
        let out_nxt_pos = if out_pos + 5 < out_len { out_pos + 5 } else { out_len };
        let exp_len = out_nxt_pos - out_pos;
        let bytes_written = to_base85_chunk(&bytes[in_pos..in_nxt_pos], &mut buffer[0..exp_len])?;    
        if bytes_written != exp_len { return Err(InvalidEncodeLength(exp_len, bytes_written)) }
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


#[cfg(test)]
mod tests {
    use super::{ decode, encode, calculate_decoding_output_length, calculate_encoding_output_length, to_base85_chunk, from_base85_chunk };
    use rand; 

    #[test]
    fn variable_round_trip () {
        for _ in 0..1000 {
            let rnd_size = rand::random::<usize>();
            let rnd_capacity: usize = rnd_size % 1000 + 1;
            let mut rnd_bytes:Vec<u8> = Vec::with_capacity(rnd_capacity);
            for _ in 0..rnd_bytes.capacity() { rnd_bytes.push(rand::random::<u8>()); }
            let enc_output = encode(&rnd_bytes);
            assert!(enc_output.is_ok(), "testing encode() using input bytes of {:02x?}", &rnd_bytes);
            let encoded = enc_output.unwrap();
            let dec_output = decode(&encoded);
            assert!(dec_output.is_ok(), "testing decode() using input of {} from bytes {:02x?} ", encoded, &rnd_bytes);
            assert_eq!(rnd_bytes, dec_output.unwrap(), "testing that decoded bytes match the original encoded bytes");
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
}
