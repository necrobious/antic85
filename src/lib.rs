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
    InvalidBase85Byte(char, usize),
    InvalidBase85Length(usize),
}

impl fmt::Debug for FromBase85Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FromBase85Error::InvalidBase85Byte(ch, idx) => write!(f, "Invalid base85 character '{}' at position {}.", ch, idx),
            FromBase85Error::InvalidBase85Length(len) => write!(f, "Invalid length {}.", len),
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
            FromBase85Error::InvalidBase85Byte(_, _) => "invalid character",
            FromBase85Error::InvalidBase85Length(_) => "invalid length",
        }
    }
}

//--- Manage b85 chunks, 4 bytes == 5 chars
pub fn to_b85_chunk (input: &[u8; 4]) -> [char; 5] {
    let mut accum: u32 = 0;
    for byte in input {
        accum = accum * 256 + (*byte as u32);
    }
    let mut output: [char; 5] = Default::default();
    for c in &mut output[..] {
        *c = ALPHABET[(accum % 85) as usize] as char;
        accum = accum / 85;
    }
    output.reverse();
    output
}

pub fn from_b85_chunk (input: &[char; 5]) -> Result<[u8; 4], FromBase85Error> {
   let mut accum: u32 = 0;
   for c in input {
       if (*c as usize) < 33 || (*c as usize) > 126 { return Err(FromBase85Error::InvalidBase85Byte(*c, 0)) }
       let index = ALPHABET_INDEX[(*c as usize) - 33];
       if index == -1 { return Err(FromBase85Error::InvalidBase85Byte(*c, 0)) }
       accum = accum * 85 + index as u32;
   }
   let mut output = [0_u8; 4];
   for b in &mut output[..] {
       *b = (accum % 256) as u8;
       accum = accum / 256;
   }
   output.reverse();
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

    if len == 0 || len % 5 != 1 { return Err(FromBase85Error::InvalidBase85Length(len)) }

    let mut ch_iter = input.chars().into_iter();

    let pad = ch_iter
        .next()
        .ok_or(FromBase85Error::InvalidBase85Length(0))
        .and_then(|c| c.to_digit(10).ok_or(FromBase85Error::InvalidBase85Byte(c, 0)))
        .and_then(|d| if d <= 3 { Ok(d) } else { Err(FromBase85Error::InvalidBase85Byte(std::char::from_u32(d).unwrap(), 0))})
        .and_then(|d| usize::try_from(d).map_err(|_| FromBase85Error::InvalidBase85Byte(std::char::from_u32(d).unwrap(), 0)))?;

    let mut output: Vec<u8> = Vec::with_capacity( (len / 5) * 4);
    let mut pos: usize = 0; 
    for ch in ch_iter.collect::<Vec<char>>().chunks(5) {
        let last = pos + 5 < len;
        let b = from_b85_chunk(&[ch[0], ch[1], ch[2], ch[3], ch[4]])
            .map_err(|err| match err {
                FromBase85Error::InvalidBase85Byte(c, p) => FromBase85Error::InvalidBase85Byte(c, p+pos),
                e => e,
            })?;
        output.extend_from_slice(if last { &b[0..(4 - pad)] } else { &b }); 
        pos = pos + 5;
    }
    Ok(output)
}
//---



#[cfg(test)]
mod tests {
    use super::{ to_b85, from_b85, to_b85_chunk, from_b85_chunk };
    use rand; 

    #[test]
    fn decode_no_padding1() {
        let t1 = "0Hello";
        let r1 = from_b85(t1);
        assert_eq!(r1, Ok(vec!(0x36, 0x60, 0xE3, 0x0D)));
        let t2 = "0World";
        let r2 = from_b85(t2);
        assert_eq!(r2, Ok(vec!(0x65, 0x6b, 0x07, 0xf9)));
    }


    #[test]
    fn decode_no_padding() {
        let t1 = "0hELLO";
        let r1 = from_b85(t1);
        assert_eq!(r1, Ok(vec!(0x86, 0x4F, 0xD2, 0x6F)));
        let t2 = "0wORLD";
        let r2 = from_b85(t2);
        assert_eq!(r2, Ok(vec!(0xB5, 0x59, 0xF7, 0x5B)));
    }

    #[test]
    fn decode_chunk() {
        let t1 = &['h','E','L','L','O'];
        let r1 = from_b85_chunk(t1);
        assert_eq!(r1, Ok([0x86, 0x4F, 0xD2, 0x6F]));
        let t2 = &['w','O','R','L','D'];
        let r2 = from_b85_chunk(t2);
        assert_eq!(r2, Ok([0xB5, 0x59, 0xF7, 0x5B]));
    }

    #[test]
    fn encode_no_padding() {
        let t1 = &[0x86, 0x4F, 0xD2, 0x6F];
        let r1 = to_b85(t1);
        assert_eq!(r1, "0hELLO".to_string());
        let t2 = &[0xB5, 0x59, 0xF7, 0x5B];
        let r2 = to_b85(t2);
        assert_eq!(r2, "0wORLD".to_string());
    }

    #[test]
    fn encode_chunk() {
        let t1 = &[0x86, 0x4F, 0xD2, 0x6F];
        let r1 = to_b85_chunk(t1);
        assert_eq!(&r1.iter().collect::<String>(), "hELLO");
        let t2 = &[0xB5, 0x59, 0xF7, 0x5B];
        let r2 = to_b85_chunk(t2);
        assert_eq!(&r2.iter().collect::<String>(), "wORLD");
    }

    #[test]
    fn random_100_round_trips () {
        for _ in 0..100 {
            let mut rnd_bytes = [0_u8; 100];
            for rnd_byte in &mut rnd_bytes[..]  {
                *rnd_byte = rand::random::<u8>();
            }
    
            let mut to_b85_output: Vec<char> = Vec::with_capacity(rnd_bytes.len() / 4 * 5);
            for c in rnd_bytes.chunks(4) {
                let cs = to_b85_chunk(&[c[0], c[1], c[2],c[3]]);
                to_b85_output.extend_from_slice(&cs);
            }
            let b85_str = to_b85_output.into_iter().collect::<String>();

            assert_eq!(125, b85_str.len());    
    
            let mut from_b85_output: Vec<u8> = Vec::with_capacity(rnd_bytes.len());
            for c in b85_str.chars().collect::<Vec<char>>().chunks(5) {
                let bs = from_b85_chunk(&[c[0], c[1], c[2],c[3],c[4]]);
                assert_eq!(true, bs.is_ok());
                from_b85_output.extend_from_slice(&bs.unwrap());
            }
    
            assert_eq!(rnd_bytes.to_vec(), from_b85_output, "testing random bytes:{:02x?} into base85: {}", &rnd_bytes, &b85_str);
        }
    }
}
