(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf
let str_of_name (u,l) = str "{%s}%s" u l 
let split_string s sep =
  let rec split accum j = 
    let i = try (String.rindex_from s j sep) with Not_found -> -1 in
    if (i = -1) then 
      let p = String.sub s 0 (j + 1) in 
      if p <> "" then p :: accum else accum
    else 
    let p = String.sub s (i + 1) (j - i) in
    let accum' = if p <> "" then p :: accum else accum in
    split accum' (i - 1)
  in
  split [] (String.length s - 1)
    
(* Error messages *)
    
let err s = failwith s
let err_data = "character data not allowed here"
let err_exp_el_end = "expected end of element"
let err_exp_data = "expected character data"
let err_wf = "document not well formed"
let err_dup n = str "duplicate element (%s)" (str_of_name n)
let err_miss_att n = str "missing attribute (%s)" n
let err_att_val v = str "invalid attribute value (\"%s\")" v
let err_invalid_cp v = str "invalid code point (\"%s\")" v
let err_empty_cps = "empty code point sequence" 
let err_exp_ucd fnd = str "expected ucd element found %s" (str_of_name fnd)
let err_invalid_cp_spec = str "invalid code point specification" 
let err_invalid_name_alias_spec = str "invalid name alias specification"

(* Code points *)
  
module Cp = struct
  type t = int
  let compare : int -> int -> int = compare 
end

type cp = Cp.t

let is_cp i = 0x0000 <= i && i <= 0x10_FFFF
let is_scalar_value i = 
  0x0000 <= i && i <= 0xD7FF && 0xE000 <= i && i <= 0x10FFFF

let cp_of_string v =                           (* parses a code point value. *)
  let is_hex c = (0x30 <= c && c <= 0x39) || (0x41 <= c && c <= 0x46) in
  let cp = ref 0 in
  for k = 0 to (String.length v) - 1 do 
    let c = Char.code v.[k] in
    if not (is_hex c) then err (err_invalid_cp v) else 
    cp := !cp * 16 + (if c <= 0x39 then c - 48 else c - 55)
  done;
  if is_cp !cp then !cp else err (err_invalid_cp v)

let cps_of_string ?(empty = false) v = (* parses a code point sequence value. *)
  if (v = "") then (if empty then [] else err err_empty_cps) else
  List.map cp_of_string (split_string v ' ')

module Cpmap = Map.Make (Cp)
   
(* Properties *)

type key =                            (* the type for property keys (names). *)
| Age
| Alphabetic
| Ascii_hex_digit
| Bidi_class
| Bidi_control
| Bidi_mirrored
| Bidi_mirroring_glyph
| Block
| Canonical_combining_class
| Cased
| Case_folding
| Case_ignorable
| Changes_when_casefolded
| Changes_when_casemapped
| Changes_when_lowercased
| Changes_when_nfkc_casefolded
| Changes_when_titlecased
| Changes_when_uppercased
| Composition_exclusion
| Dash
| Decomposition_mapping
| Decomposition_type
| Default_ignorable_code_point
| Deprecated
| Diacritic
| East_asian_width
| Expands_on_nfc
| Expands_on_nfd
| Expands_on_nfkc
| Expands_on_nfkd
| Extender
| Fc_nfkc_closure
| Full_composition_exclusion
| General_category
| Grapheme_base
| Grapheme_cluster_break
| Grapheme_extend
| Grapheme_link
| Hangul_syllable_type
| Hex_digit
| Hyphen
| Id_continue
| Id_start
| Ideographic
| Ids_binary_operator
| Ids_trinary_operator
| Indic_syllabic_category
| Indic_matra_category
| Iso_comment
| Jamo_short_name
| Join_control
| Joining_group
| Joining_type
| Line_break
| Logical_order_exception
| Lowercase
| Lowercase_mapping
| Math
| Name
| Name_alias
| Nfc_quick_check
| Nfd_quick_check
| Nfkc_quick_check
| Nfkc_casefold
| Nfkd_quick_check
| Noncharacter_code_point
| Numeric_type
| Numeric_value
| Other_alphabetic
| Other_default_ignorable_code_point
| Other_grapheme_extend
| Other_id_continue
| Other_id_start
| Other_lowercase
| Other_math
| Other_uppercase
| Pattern_syntax
| Pattern_white_space
| Quotation_mark
| Radical
| Script
| Script_extensions
| Sentence_break
| Simple_case_folding
| Simple_lowercase_mapping
| Simple_titlecase_mapping
| Simple_uppercase_mapping
| Soft_dotted
| Sterm
| Terminal_punctuation
| Titlecase_mapping
| UAX_42_element
| Unicode_1_name
| Unified_ideograph
| Uppercase
| Uppercase_mapping
| Variation_selector
| White_space
| Word_break
| Xid_continue
| Xid_start
(* Unihan *)
| KAccountingNumeric
| KAlternateHanYu
| KAlternateJEF
| KAlternateKangXi
| KAlternateMorohashi
| KBigFive
| KCCCII
| KCNS1986
| KCNS1992
| KCangjie
| KCantonese
| KCheungBauer
| KCheungBauerIndex
| KCihaiT
| KCompatibilityVariant
| KCowles
| KDaeJaweon
| KDefinition
| KEACC
| KFenn
| KFennIndex
| KFourCornerCode
| KFrequency
| KGB0
| KGB1
| KGB3
| KGB5
| KGB7
| KGB8
| KGradeLevel
| KGSR
| KHangul
| KHanYu
| KHanyuPinlu
| KHanyuPinyin
| KHDZRadBreak
| KHKGlyph
| KHKSCS
| KIBMJapan
| KIICore
| KIRGDaeJaweon
| KIRGDaiKanwaZiten
| KIRGHanyuDaZidian
| KIRGKangXi
| KIRG_GSource
| KIRG_HSource
| KIRG_JSource
| KIRG_KPSource
| KIRG_KSource
| KIRG_MSource
| KIRG_TSource
| KIRG_USource
| KIRG_VSource
| KJHJ
| KJIS0213
| KJapaneseKun
| KJapaneseOn
| KJis0
| KJis1
| KKPS0
| KKPS1
| KKSC0
| KKSC1
| KKangXi
| KKarlgren
| KKorean
| KLau
| KMainlandTelegraph
| KMandarin
| KMatthews
| KMeyerWempe
| KMorohashi
| KNelson
| KOtherNumeric
| KPhonetic
| KPrimaryNumeric
| KPseudoGB1
| KRSAdobe_Japan1_6
| KRSJapanese
| KRSKanWa
| KRSKangXi
| KRSKorean
| KRSMerged
| KRSUnicode
| KSBGY
| KSemanticVariant
| KSimplifiedVariant
| KSpecializedSemanticVariant
| KTaiwanTelegraph
| KTang
| KTotalStrokes
| KTraditionalVariant
| KVietnamese
| KXHC1983
| KWubi
| KXerox
| KZVariant 
| Other of (string * string)                           (* expanded XML name. *)

type script = 
[ `Arab | `Armi | `Armn | `Avst | `Bali | `Bamu | `Batk | `Beng | `Bopo 
| `Brah | `Brai | `Bugi | `Buhd | `Cakm | `Cans | `Cari | `Cham | `Cher 
| `Copt | `Cprt | `Cyrl | `Deva | `Dsrt | `Egyp | `Ethi | `Geor | `Glag 
| `Goth | `Grek | `Gujr | `Guru | `Hang | `Hani | `Hano | `Hebr | `Hira 
| `Hrkt | `Ital | `Java | `Kali | `Kana | `Khar | `Khmr | `Knda | `Kthi
| `Lana | `Laoo | `Latn | `Lepc | `Limb | `Linb | `Lisu | `Lyci | `Lydi
| `Mand | `Merc | `Mero | `Mlym | `Mong | `Mtei | `Mymr | `Nkoo | `Ogam 
| `Olck | `Orkh | `Orya | `Osma | `Phag | `Phli | `Phnx | `Plrd | `Prti 
| `Qaai | `Rjng | `Runr | `Samr | `Sarb | `Saur | `Shaw | `Shrd | `Sinh 
| `Sora | `Sund | `Sylo | `Syrc | `Tagb | `Takr | `Tale | `Talu | `Taml 
| `Tavt | `Telu | `Tfng | `Tglg | `Thaa | `Thai | `Tibt | `Ugar | `Vaii
| `Xpeo | `Xsux | `Yiii | `Zinh | `Zyyy | `Zzzz ]

type value =                                (* the type for property values. *)
| Age_v of [ `Version of int * int | `Unassigned ]
| Block_v of 
    [ `Aegean_Numbers | `Alchemical | `Alphabetic_PF | `Ancient_Greek_Music 
    | `Ancient_Greek_Numbers | `Ancient_Symbols | `Arabic | `Arabic_Ext_A 
    | `Arabic_Math | `Arabic_PF_A | `Arabic_PF_B | `Arabic_Sup | `Armenian 
    | `Arrows | `ASCII | `Avestan | `Balinese | `Bamum | `Bamum_Sup | `Batak 
    | `Bengali | `Block_Elements | `Bopomofo | `Bopomofo_Ext | `Box_Drawing 
    | `Brahmi | `Braille | `Buginese | `Buhid | `Byzantine_Music | `Carian 
    | `Chakma | `Cham | `Cherokee | `CJK | `CJK_Compat | `CJK_Compat_Forms 
    | `CJK_Compat_Ideographs | `CJK_Compat_Ideographs_Sup | `CJK_Ext_A 
    | `CJK_Ext_B | `CJK_Ext_C | `CJK_Ext_D | `CJK_Radicals_Sup | `CJK_Strokes 
    | `CJK_Symbols | `Compat_Jamo | `Control_Pictures | `Coptic | `Counting_Rod 
    | `Cuneiform | `Cuneiform_Numbers | `Currency_Symbols | `Cypriot_Syllabary 
    | `Cyrillic | `Cyrillic_Ext_A | `Cyrillic_Ext_B | `Cyrillic_Sup | `Deseret 
    | `Devanagari | `Devanagari_Ext | `Diacriticals | `Diacriticals_For_Symbols 
    | `Diacriticals_Sup | `Dingbats | `Domino | `Egyptian_Hieroglyphs 
    | `Emoticons | `Enclosed_Alphanum | `Enclosed_Alphanum_Sup | `Enclosed_CJK 
    | `Enclosed_Ideographic_Sup | `Ethiopic | `Ethiopic_Ext | `Ethiopic_Ext_A 
    | `Ethiopic_Sup | `Geometric_Shapes | `Georgian | `Georgian_Sup 
    | `Glagolitic | `Gothic | `Greek | `Greek_Ext | `Gujarati | `Gurmukhi 
    | `Half_And_Full_Forms | `Half_Marks | `Hangul | `Hanunoo | `Hebrew 
    | `High_PU_Surrogates | `High_Surrogates | `Hiragana | `IDC 
    | `Imperial_Aramaic | `Indic_Number_Forms | `Inscriptional_Pahlavi 
    | `Inscriptional_Parthian | `IPA_Ext | `Jamo | `Jamo_Ext_A | `Jamo_Ext_B 
    | `Javanese | `Kaithi | `Kana_Sup | `Kanbun | `Kangxi | `Kannada 
    | `Katakana | `Katakana_Ext | `Kayah_Li | `Kharoshthi | `Khmer 
    | `Khmer_Symbols | `Lao | `Latin_1_Sup | `Latin_Ext_A 
    | `Latin_Ext_Additional | `Latin_Ext_B | `Latin_Ext_C | `Latin_Ext_D 
    | `Lepcha | `Letterlike_Symbols | `Limbu | `Linear_B_Ideograms 
    | `Linear_B_Syllabary | `Lisu | `Low_Surrogates | `Lycian | `Lydian 
    | `Mahjong | `Malayalam | `Mandaic | `Math_Alphanum | `Math_Operators 
    | `Meetei_Mayek | `Meetei_Mayek_Ext | `Meroitic_Cursive 
    | `Meroitic_Hieroglyphs | `Miao | `Misc_Arrows | `Misc_Math_Symbols_A 
    | `Misc_Math_Symbols_B | `Misc_Pictographs | `Misc_Symbols 
    | `Misc_Technical | `Modifier_Letters | `Modifier_Tone_Letters 
    | `Mongolian | `Music | `Myanmar | `Myanmar_Ext_A
    | `NB | `New_Tai_Lue | `NKo | `Number_Forms | `OCR | `Ogham | `Ol_Chiki 
    | `Old_Italic | `Old_Persian | `Old_South_Arabian | `Old_Turkic | `Oriya 
    | `Osmanya | `Phags_Pa | `Phaistos | `Phoenician
    | `Phonetic_Ext | `Phonetic_Ext_Sup | `Playing_Cards | `PUA 
    | `Punctuation | `Rejang | `Rumi | `Runic | `Samaritan | `Saurashtra 
    | `Sharada | `Shavian | `Sinhala | `Small_Forms | `Sora_Sompeng 
    | `Specials | `Sundanese | `Sundanese_Sup | `Sup_Arrows_A | `Sup_Arrows_B 
    | `Sup_Math_Operators | `Sup_PUA_A | `Sup_PUA_B | `Sup_Punctuation 
    | `Super_And_Sub | `Syloti_Nagri | `Syriac | `Tagalog | `Tagbanwa | `Tags 
    | `Tai_Le | `Tai_Tham | `Tai_Viet | `Tai_Xuan_Jing | `Takri | `Tamil 
    | `Telugu | `Thaana | `Thai | `Tibetan | `Tifinagh | `Transport_And_Map 
    | `UCAS | `UCAS_Ext | `Ugaritic | `Vai | `Vedic_Ext | `Vertical_Forms 
    | `VS | `VS_Sup | `Yi_Radicals | `Yi_Syllables | `Yijing ] 
| Bidi_class_v of
    [ `AL | `AN | `B | `BN | `CS | `EN | `ES | `ET | `L | `LRE | `LRO | `NSM 
    | `ON | `PDF | `R | `RLE | `RLO | `S | `WS ]
| Bool_v of bool
| Bool_maybe_v of [ `True | `False | `Maybe ]
| Cp_v of cp
| Cp_map_v of [ `Self | `Cp of cp ]
| Cp_opt_v of cp option
| Decomposition_type_v of
    [ `Can  | `Com | `Enc | `Fin  | `Font | `Fra | `Init | `Iso | `Med | `Nar  
    | `Nb   | `Sml | `Sqr  | `Sub | `Sup | `Vert | `Wide | `None ]
| East_asian_width_v of [ `A | `F | `H | `N | `Na | `W ]
| General_category_v of 
    [ `Lu | `Ll | `Lt | `Lm | `Lo | `Mn | `Mc | `Me | `Nd | `Nl | `No | `Pc 
    | `Pd | `Ps | `Pe | `Pi | `Pf | `Po | `Sm | `Sc | `Sk | `So | `Zs | `Zl 
    | `Zp | `Cc | `Cf | `Cs | `Co | `Cn ]
| Grapheme_cluster_break_v of
    [`CN | `CR | `EX | `L  | `LF |  `LV | `LVT | `PP | `SM | `T  | `V  | `XX ]
| Hangul_syllable_type_v of [ `L | `LV | `LVT | `T | `V | `NA ]
| Int_v of int
| Indic_syllabic_category_v of 
    [ `Bindu | `Visarga | `Avagraha | `Nukta | `Virama | `Vowel_Independent 
    | `Vowel_Dependent | `Vowel | `Consonant_Placeholder | `Consonant 
    | `Consonant_Dead | `Consonant_Repha | `Consonant_Subjoined 
    | `Consonant_Medial | `Consonant_Final | `Consonant_Head_Letter 
    | `Modifying_Letter | `Tone_Letter | `Tone_Mark | `Register_Shifter 
    | `Other ]
| Indic_matra_category_v of 
    [ `Right | `Left | `Visual_Order_Left | `Left_And_Right | `Top | `Bottom 
    | `Top_And_Bottom | `Top_And_Right | `Top_And_Left 
    | `Top_And_Left_And_Right |  `Bottom_And_Right | `Top_And_Bottom_And_Right 
    | `Overstruck | `Invisible | `NA ]
| Joining_group_v of 
    [ `Ain | `Alaph | `Alef | `Alef_Maqsurah | `Beh | `Beth 
    | `Burushaski_Yeh_Barree | `Dal | `Dalath_Rish | `E | `Farsi_Yeh | `Fe 
    | `Feh | `Final_Semkath | `Gaf | `Gamal | `Hah | `Hamza_On_Heh_Goal | `He 
    | `Heh | `Heh_Goal | `Heth | `Kaf | `Kaph | `Khaph | `Knotted_Heh | `Lam 
    | `Lamadh | `Meem | `Mim | `No_Joining_Group | `Noon | `Nun | `Nya 
    | `Pe | `Qaf | `Qaph | `Reh | `Reversed_Pe | `Rohingya_Yeh | `Sad | `Sadhe 
    | `Seen | `Semkath | `Shin | `Swash_Kaf | `Syriac_Waw | `Tah | `Taw 
    | `Teh_Marbuta | `Teh_Marbuta_Goal | `Teth | `Waw | `Yeh | `Yeh_Barree 
    | `Yeh_With_Tail | `Yudh | `Yudh_He | `Zain | `Zhain ]
| Joining_type_v of [ `U | `C | `T | `D | `L | `R ]
| Line_break_v of
    [ `AI | `AL | `B2 | `BA | `BB | `BK | `CB | `CJ | `CL | `CM | `CP | `CR 
    | `EX | `GL | `H2 | `H3 | `HL | `HY | `ID | `IN | `IS | `JL | `JT | `JV 
    | `LF | `NL | `NS | `NU | `OP | `PO | `PR | `QU | `SA | `SG | `SP | `SY 
    | `WJ | `XX | `ZW ]
| Name_v of [`Pattern of string | `Name of string ]
| Name_alias_v of 
    (string * [`Abbreviation | `Alternate | `Control | `Correction | `Figment]) 
      list
| Numeric_type_v of [ `None | `De | `Di | `Nu ]
| Numeric_value_v of [`NaN | `Frac of int * int | `Num of int64 ]
| Script_v of script
| Script_extensions_v of script list
| Sentence_break_v of
    [ `AT | `CL | `CR | `EX | `FO | `LE | `LF | `LO | `NU | `SC | `SE | `SP 
    | `ST | `UP | `XX ]
| Cps_v of cp list
| Cps_map_v of [ `Self | `Cps of cp list ]
| String_v of string
| UAX_42_element_v of [ `Reserved | `Noncharacter | `Surrogate | `Char ] 
| Word_break_v of
    [ `CR | `EX | `Extend | `FO | `KA | `LE | `LF | `MB | `ML | `MN | `NL 
    | `NU | `XX ]

(* property value projection *)

let o_age = function Age_v v -> v | _ -> assert false
let o_bidi_class = function Bidi_class_v v -> v | _ -> assert false
let o_block = function Block_v v -> v | _ -> assert false
let o_bool = function Bool_v v -> v | _ -> assert false
let o_bool_maybe = function Bool_maybe_v v -> v | _ -> assert false
let o_cp = function Cp_v v -> v | _ -> assert false
let o_cp_map = function Cp_map_v v -> v | _ -> assert false
let o_cp_opt = function Cp_opt_v v -> v | _ -> assert false
let o_decomposition_type = 
  function Decomposition_type_v v -> v | _ -> assert false

let o_east_asian_width = function East_asian_width_v v -> v | _ -> assert false
let o_general_category = function General_category_v v -> v | _ -> assert false
let o_grapheme_cluster_break = 
  function Grapheme_cluster_break_v v -> v | _ -> assert false
    
let o_hangul_syllable_type = 
  function Hangul_syllable_type_v v -> v | _ -> assert false
    
let o_int = function Int_v v -> v | _ -> assert false
let o_indic_syllabic_category = 
  function Indic_syllabic_category_v v -> v | _ -> assert false

let o_indic_matra_category = 
  function Indic_matra_category_v v -> v | _ -> assert false

let o_joining_group = function Joining_group_v v -> v | _ -> assert false
let o_joining_type = function Joining_type_v v -> v | _ -> assert false
let o_line_break = function Line_break_v v -> v | _ -> assert false
let o_name = function Name_v v -> v | _ -> assert false
let o_name_alias = function Name_alias_v v -> v | _ -> assert false
let o_numeric_type = function Numeric_type_v v -> v | _ -> assert false
let o_numeric_value = function Numeric_value_v v -> v | _ -> assert false
let o_script = function Script_v v -> v | _ -> assert false
let o_script_extensions = 
  function Script_extensions_v v -> v | _ -> assert false

let o_sentence_break = function Sentence_break_v v -> v | _ -> assert false
let o_cps = function Cps_v v -> v | _ -> assert false
let o_cps_map = function Cps_map_v v -> v | _ -> assert false
let o_string = function String_v v -> v | _ -> assert false
let o_uax_42_element = function UAX_42_element_v v -> v | _ -> assert false
let o_word_break = function Word_break_v v -> v | _ -> assert false

(* property value injection *)

let i_age v = Age_v begin match v with 
| "unassigned" -> `Unassigned 
| v -> 
    try match List.map int_of_string (split_string v '.') with
    | [v1; v2;] -> `Version (v1, v2)
    | _ -> failwith ""
    with Failure _ -> err (err_att_val v)
end

let i_bidi_class v = Bidi_class_v begin match v with 
| "AL" -> `AL | "AN" -> `AN | "B" -> `B | "BN" -> `BN | "CS" -> `CS 
| "EN" -> `EN | "ES" -> `ES | "ET" -> `ET | "L" -> `L | "LRE" -> `LRE 
| "LRO" -> `LRO | "NSM" -> `NSM | "ON" -> `ON | "PDF" -> `PDF | "R" -> `R 
| "RLE" -> `RLE | "RLO" -> `RLO | "S" -> `S | "WS" -> `WS 
| v -> err (err_att_val v)
end

let i_block v = Block_v begin match v with 
| "Aegean_Numbers" -> `Aegean_Numbers | "Alchemical" -> `Alchemical 
| "Alphabetic_PF" -> `Alphabetic_PF 
| "Ancient_Greek_Music" -> `Ancient_Greek_Music 
| "Ancient_Greek_Numbers" -> `Ancient_Greek_Numbers 
| "Ancient_Symbols" -> `Ancient_Symbols | "Arabic" -> `Arabic 
| "Arabic_Ext_A" -> `Arabic_Ext_A | "Arabic_Math" -> `Arabic_Math 
| "Arabic_PF_A" -> `Arabic_PF_A | "Arabic_PF_B" -> `Arabic_PF_B 
| "Arabic_Sup" -> `Arabic_Sup | "Armenian" -> `Armenian | "Arrows" -> `Arrows 
| "ASCII" -> `ASCII | "Avestan" -> `Avestan | "Balinese" -> `Balinese 
| "Bamum" -> `Bamum | "Bamum_Sup" -> `Bamum_Sup | "Batak" -> `Batak 
| "Bengali" -> `Bengali | "Block_Elements" -> `Block_Elements 
| "Bopomofo" -> `Bopomofo | "Bopomofo_Ext" -> `Bopomofo_Ext 
| "Box_Drawing" -> `Box_Drawing | "Brahmi" -> `Brahmi | "Braille" -> `Braille 
| "Buginese" -> `Buginese | "Buhid" -> `Buhid 
| "Byzantine_Music" -> `Byzantine_Music | "Carian" -> `Carian 
| "Chakma" -> `Chakma | "Cham" -> `Cham | "Cherokee" -> `Cherokee 
| "CJK" -> `CJK | "CJK_Compat" -> `CJK_Compat 
| "CJK_Compat_Forms" -> `CJK_Compat_Forms 
| "CJK_Compat_Ideographs" -> `CJK_Compat_Ideographs 
| "CJK_Compat_Ideographs_Sup" -> `CJK_Compat_Ideographs_Sup 
| "CJK_Ext_A" -> `CJK_Ext_A | "CJK_Ext_B" -> `CJK_Ext_B 
| "CJK_Ext_C" -> `CJK_Ext_C | "CJK_Ext_D" -> `CJK_Ext_D 
| "CJK_Radicals_Sup" -> `CJK_Radicals_Sup | "CJK_Strokes" -> `CJK_Strokes 
| "CJK_Symbols" -> `CJK_Symbols | "Compat_Jamo" -> `Compat_Jamo 
| "Control_Pictures" -> `Control_Pictures | "Coptic" -> `Coptic 
| "Counting_Rod" -> `Counting_Rod | "Cuneiform" -> `Cuneiform 
| "Cuneiform_Numbers" -> `Cuneiform_Numbers 
| "Currency_Symbols" -> `Currency_Symbols 
| "Cypriot_Syllabary" -> `Cypriot_Syllabary | "Cyrillic" -> `Cyrillic 
| "Cyrillic_Ext_A" -> `Cyrillic_Ext_A 
| "Cyrillic_Ext_B" -> `Cyrillic_Ext_B 
| "Cyrillic_Sup" -> `Cyrillic_Sup | "Deseret" -> `Deseret 
| "Devanagari" -> `Devanagari | "Devanagari_Ext" -> `Devanagari_Ext 
| "Diacriticals" -> `Diacriticals 
| "Diacriticals_For_Symbols" -> `Diacriticals_For_Symbols 
| "Diacriticals_Sup" -> `Diacriticals_Sup 
| "Dingbats" -> `Dingbats | "Domino" -> `Domino 
| "Egyptian_Hieroglyphs" -> `Egyptian_Hieroglyphs | "Emoticons" -> `Emoticons 
| "Enclosed_Alphanum" -> `Enclosed_Alphanum 
| "Enclosed_Alphanum_Sup" -> `Enclosed_Alphanum_Sup 
| "Enclosed_CJK" -> `Enclosed_CJK 
| "Enclosed_Ideographic_Sup" -> `Enclosed_Ideographic_Sup 
| "Ethiopic" -> `Ethiopic 
| "Ethiopic_Ext" -> `Ethiopic_Ext | "Ethiopic_Ext_A" -> `Ethiopic_Ext_A 
| "Ethiopic_Sup" -> `Ethiopic_Sup | "Geometric_Shapes" -> `Geometric_Shapes 
| "Georgian" -> `Georgian | "Georgian_Sup" -> `Georgian_Sup 
| "Glagolitic" -> `Glagolitic | "Gothic" -> `Gothic 
| "Greek" -> `Greek | "Greek_Ext" -> `Greek_Ext 
| "Gujarati" -> `Gujarati | "Gurmukhi" -> `Gurmukhi 
| "Half_And_Full_Forms" -> `Half_And_Full_Forms | "Half_Marks" -> `Half_Marks 
| "Hangul" -> `Hangul | "Hanunoo" -> `Hanunoo | "Hebrew" -> `Hebrew 
| "High_PU_Surrogates" -> `High_PU_Surrogates 
| "High_Surrogates" -> `High_Surrogates | "Hiragana" -> `Hiragana 
| "IDC" -> `IDC | "Imperial_Aramaic" -> `Imperial_Aramaic 
| "Indic_Number_Forms" -> `Indic_Number_Forms 
| "Inscriptional_Pahlavi" -> `Inscriptional_Pahlavi 
| "Inscriptional_Parthian" -> `Inscriptional_Parthian | "IPA_Ext" -> `IPA_Ext 
| "Jamo" -> `Jamo | "Jamo_Ext_A" -> `Jamo_Ext_A | "Jamo_Ext_B" -> `Jamo_Ext_B 
| "Javanese" -> `Javanese | "Kaithi" -> `Kaithi | "Kana_Sup" -> `Kana_Sup 
| "Kanbun" -> `Kanbun | "Kangxi" -> `Kangxi | "Kannada" -> `Kannada 
| "Katakana" -> `Katakana | "Katakana_Ext" -> `Katakana_Ext 
| "Kayah_Li" -> `Kayah_Li | "Kharoshthi" -> `Kharoshthi | "Khmer" -> `Khmer 
| "Khmer_Symbols" -> `Khmer_Symbols | "Lao" -> `Lao 
| "Latin_1_Sup" -> `Latin_1_Sup | "Latin_Ext_A" -> `Latin_Ext_A 
| "Latin_Ext_Additional" -> `Latin_Ext_Additional 
| "Latin_Ext_B" -> `Latin_Ext_B | "Latin_Ext_C" -> `Latin_Ext_C 
| "Latin_Ext_D" -> `Latin_Ext_D | "Lepcha" -> `Lepcha 
| "Letterlike_Symbols" -> `Letterlike_Symbols | "Limbu" -> `Limbu 
| "Linear_B_Ideograms" -> `Linear_B_Ideograms 
| "Linear_B_Syllabary" -> `Linear_B_Syllabary 
| "Lisu" -> `Lisu | "Low_Surrogates" -> `Low_Surrogates 
| "Lycian" -> `Lycian | "Lydian" -> `Lydian | "Mahjong" -> `Mahjong 
| "Malayalam" -> `Malayalam | "Mandaic" -> `Mandaic 
| "Math_Alphanum" -> `Math_Alphanum | "Math_Operators" -> `Math_Operators 
| "Meetei_Mayek" -> `Meetei_Mayek | "Meetei_Mayek_Ext" -> `Meetei_Mayek_Ext 
| "Meroitic_Cursive" -> `Meroitic_Cursive 
| "Meroitic_Hieroglyphs" -> `Meroitic_Hieroglyphs | "Miao" -> `Miao 
| "Misc_Arrows" -> `Misc_Arrows 
| "Misc_Math_Symbols_A" -> `Misc_Math_Symbols_A 
| "Misc_Math_Symbols_B" -> `Misc_Math_Symbols_B 
| "Misc_Pictographs" -> `Misc_Pictographs | "Misc_Symbols" -> `Misc_Symbols 
| "Misc_Technical" -> `Misc_Technical | "Modifier_Letters" -> `Modifier_Letters
| "Modifier_Tone_Letters" -> `Modifier_Tone_Letters | "Mongolian" -> `Mongolian
| "Music" -> `Music | "Myanmar" -> `Myanmar | "Myanmar_Ext_A" -> `Myanmar_Ext_A
| "NB" -> `NB | "New_Tai_Lue" -> `New_Tai_Lue | "NKo" -> `NKo 
| "Number_Forms" -> `Number_Forms | "OCR" -> `OCR | "Ogham" -> `Ogham 
| "Ol_Chiki" -> `Ol_Chiki | "Old_Italic" -> `Old_Italic 
| "Old_Persian" -> `Old_Persian | "Old_South_Arabian" -> `Old_South_Arabian 
| "Old_Turkic" -> `Old_Turkic | "Oriya" -> `Oriya | "Osmanya" -> `Osmanya 
| "Phags_Pa" -> `Phags_Pa | "Phaistos" -> `Phaistos 
| "Phoenician" -> `Phoenician | "Phonetic_Ext" -> `Phonetic_Ext 
| "Phonetic_Ext_Sup" -> `Phonetic_Ext_Sup | "Playing_Cards" -> `Playing_Cards 
| "PUA" -> `PUA | "Punctuation" -> `Punctuation | "Rejang" -> `Rejang 
| "Rumi" -> `Rumi | "Runic" -> `Runic | "Samaritan" -> `Samaritan 
| "Saurashtra" -> `Saurashtra | "Sharada" -> `Sharada | "Shavian" -> `Shavian 
| "Sinhala" -> `Sinhala | "Small_Forms" -> `Small_Forms 
| "Sora_Sompeng" -> `Sora_Sompeng | "Specials" -> `Specials 
| "Sundanese" -> `Sundanese | "Sundanese_Sup" -> `Sundanese_Sup 
| "Sup_Arrows_A" -> `Sup_Arrows_A | "Sup_Arrows_B" -> `Sup_Arrows_B 
| "Sup_Math_Operators" -> `Sup_Math_Operators | "Sup_PUA_A" -> `Sup_PUA_A 
| "Sup_PUA_B" -> `Sup_PUA_B | "Sup_Punctuation" -> `Sup_Punctuation 
| "Super_And_Sub" -> `Super_And_Sub | "Syloti_Nagri" -> `Syloti_Nagri 
| "Syriac" -> `Syriac | "Tagalog" -> `Tagalog | "Tagbanwa" -> `Tagbanwa 
| "Tags" -> `Tags | "Tai_Le" -> `Tai_Le | "Tai_Tham" -> `Tai_Tham 
| "Tai_Viet" -> `Tai_Viet | "Tai_Xuan_Jing" -> `Tai_Xuan_Jing 
| "Takri" -> `Takri | "Tamil" -> `Tamil | "Telugu" -> `Telugu 
| "Thaana" -> `Thaana | "Thai" -> `Thai | "Tibetan" -> `Tibetan 
| "Tifinagh" -> `Tifinagh | "Transport_And_Map" -> `Transport_And_Map 
| "UCAS" -> `UCAS | "UCAS_Ext" -> `UCAS_Ext | "Ugaritic" -> `Ugaritic 
| "Vai" -> `Vai | "Vedic_Ext" -> `Vedic_Ext 
| "Vertical_Forms" -> `Vertical_Forms | "VS" -> `VS | "VS_Sup" -> `VS_Sup 
| "Yi_Radicals" -> `Yi_Radicals | "Yi_Syllables" -> `Yi_Syllables 
| "Yijing" -> `Yijing 
| v -> err (err_att_val v) 
end
  
let i_bool v = Bool_v begin match v with
| "Y" -> true | "N" -> false 
| v -> err (err_att_val v)
end
  
let i_bool_maybe v = Bool_maybe_v begin match v with
| "Y" -> `True | "N" -> `False | "M" -> `Maybe 
| v -> err (err_att_val v)
end
  
let i_cp v = Cp_v (cp_of_string v)
let i_cp_map v = 
  if v = "#" then Cp_map_v `Self else Cp_map_v (`Cp (cp_of_string v))
  
let i_cp_opt v = 
  if v = "" then Cp_opt_v None else Cp_opt_v (Some (cp_of_string v))

let i_cps ?empty v = Cps_v (cps_of_string ?empty v)
let i_cps_map ?empty v = 
  if v = "#" then Cps_map_v `Self else Cps_map_v (`Cps (cps_of_string ?empty v))

let i_decomposition_type v = Decomposition_type_v begin match v with
| "can" -> `Can  | "com" -> `Com | "enc" -> `Enc | "fin" -> `Fin  
| "font" -> `Font | "fra" -> `Fra | "init" -> `Init | "iso" -> `Iso 
| "med" -> `Med | "nar" -> `Nar  | "nb" -> `Nb   | "sml" -> `Sml 
| "sqr" -> `Sqr  | "sub" -> `Sub | "sup" -> `Sup | "vert" -> `Vert 
| "wide" -> `Wide | "none" -> `None
| v -> err (err_att_val v)
end
  
let i_east_asian_width v = East_asian_width_v begin match v with
| "A" -> `A | "F" -> `F | "H" -> `H | "N" -> `N | "Na" -> `Na | "W" -> `W
| v -> err (err_att_val v)
end
  
let i_general_category v = General_category_v begin match v with
| "Lu" -> `Lu | "Ll" -> `Ll | "Lt" -> `Lt | "Lm" -> `Lm | "Lo" -> `Lo
| "Mn" -> `Mn | "Mc" -> `Mc | "Me" -> `Me | "Nd" -> `Nd | "Nl" -> `Nl 
| "No" -> `No | "Pc" -> `Pc | "Pd" -> `Pd | "Ps" -> `Ps | "Pe" -> `Pe 
| "Pi" -> `Pi | "Pf" -> `Pf | "Po" -> `Po | "Sm" -> `Sm | "Sc" ->`Sc 
| "Sk" -> `Sk | "So" -> `So | "Zs" -> `Zs | "Zl" -> `Zl | "Zp" -> `Zp
| "Cc" -> `Cc | "Cf" -> `Cf | "Cs" -> `Cs | "Co" -> `Co | "Cn" -> `Cn
| v -> err (err_att_val v)
end
  
let i_grapheme_cluster_break v = Grapheme_cluster_break_v begin match v with
| "CN" -> `CN | "CR" -> `CR | "EX" -> `EX | "L" -> `L  | "LF" -> `LF 
| "LV" -> `LV | "LVT" -> `LVT | "PP" -> `PP | "SM" -> `SM | "T" -> `T  
| "V" -> `V  | "XX" -> `XX 
| v -> err (err_att_val v)
end

let i_hangul_syllable_type v = Hangul_syllable_type_v begin match v with
| "L" -> `L | "LV" -> `LV | "LVT" -> `LVT | "T" -> `T | "V" -> `V | "NA" -> `NA
| v -> err (err_att_val v)
end

let i_int v = try Int_v (int_of_string v) with Failure _ -> err (err_att_val v)
let i_indic_syllabic_category v = Indic_syllabic_category_v begin match v with
| "Bindu" -> `Bindu | "Visarga" -> `Visarga | "Avagraha" -> `Avagraha 
| "Nukta" -> `Nukta | "Virama" -> `Virama 
| "Vowel_Independent" -> `Vowel_Independent 
| "Vowel_Dependent" -> `Vowel_Dependent
| "Vowel" -> `Vowel | "Consonant_Placeholder" -> `Consonant_Placeholder 
| "Consonant" -> `Consonant | "Consonant_Dead" -> `Consonant_Dead 
| "Consonant_Repha" -> `Consonant_Repha 
| "Consonant_Subjoined" -> `Consonant_Subjoined
| "Consonant_Medial" -> `Consonant_Medial 
| "Consonant_Final" -> `Consonant_Final
| "Consonant_Head_Letter" -> `Consonant_Head_Letter 
| "Modifying_Letter" -> `Modifying_Letter
| "Tone_Letter" -> `Tone_Letter | "Tone_Mark" -> `Tone_Mark
| "Register_Shifter" -> `Register_Shifter  | "Other" -> `Other
| v -> err (err_att_val v)
end

let i_indic_matra_category v = Indic_matra_category_v begin match v with 
| "Right" -> `Right | "Left" -> `Left 
| "Visual_Order_Left" -> `Visual_Order_Left 
| "Left_And_Right" -> `Left_And_Right | "Top" -> `Top | "Bottom" -> `Bottom 
| "Top_And_Bottom" -> `Top_And_Bottom | "Top_And_Right" -> `Top_And_Right 
| "Top_And_Left" -> `Top_And_Left 
| "Top_And_Left_And_Right" -> `Top_And_Left_And_Right 
| "Bottom_And_Right" -> `Bottom_And_Right 
| "Top_And_Bottom_And_Right" -> `Top_And_Bottom_And_Right 
| "Overstruck" -> `Overstruck | "Invisible" -> `Invisible | "NA" -> `NA
| v -> err (err_att_val v)
end

let i_joining_group v = Joining_group_v begin match v with
| "Ain" -> `Ain | "Alaph" -> `Alaph | "Alef" -> `Alef 
| "Alef_Maqsurah" -> `Alef_Maqsurah | "Beh" -> `Beh | "Beth" -> `Beth 
| "Burushaski_Yeh_Barree" -> `Burushaski_Yeh_Barree | "Dal" -> `Dal 
| "Dalath_Rish" -> `Dalath_Rish | "E" -> `E | "Farsi_Yeh" -> `Farsi_Yeh 
| "Fe" -> `Fe | "Feh" -> `Feh | "Final_Semkath" -> `Final_Semkath 
| "Gaf" -> `Gaf | "Gamal" -> `Gamal | "Hah" -> `Hah 
| "Hamza_On_Heh_Goal" -> `Hamza_On_Heh_Goal | "He" -> `He | "Heh" -> `Heh 
| "Heh_Goal" -> `Heh_Goal | "Heth" -> `Heth  | "Kaf" -> `Kaf 
| "Kaph" -> `Kaph | "Khaph" -> `Khaph | "Knotted_Heh" -> `Knotted_Heh 
| "Lam" -> `Lam | "Lamadh" -> `Lamadh | "Meem" -> `Meem | "Mim" -> `Mim 
| "No_Joining_Group" -> `No_Joining_Group | "Noon" -> `Noon | "Nun" -> `Nun 
| "Nya" -> `Nya | "Pe" -> `Pe | "Qaf" -> `Qaf | "Qaph" -> `Qaph | "Reh" -> `Reh 
| "Reversed_Pe" -> `Reversed_Pe | "Rohingya_Yeh" -> `Rohingya_Yeh
| "Sad" -> `Sad | "Sadhe" -> `Sadhe | "Seen" -> `Seen | "Semkath" -> `Semkath 
| "Shin" -> `Shin | "Swash_Kaf" -> `Swash_Kaf | "Syriac_Waw" -> `Syriac_Waw 
| "Tah" -> `Tah | "Taw" -> `Taw | "Teh_Marbuta" -> `Teh_Marbuta 
| "Teh_Marbuta_Goal" -> `Teh_Marbuta_Goal | "Teth" -> `Teth | "Waw" -> `Waw 
| "Yeh" -> `Yeh | "Yeh_Barree" -> `Yeh_Barree 
| "Yeh_With_Tail" -> `Yeh_With_Tail | "Yudh" -> `Yudh | "Yudh_He" -> `Yudh_He 
| "Zain" -> `Zain | "Zhain" -> `Zhain
| v -> err (err_att_val v)
end

let i_joining_type v = Joining_type_v begin match v with
| "U" -> `U | "C" -> `C | "T" -> `T | "D" -> `D | "L" -> `L | "R" -> `R
| v -> err (err_att_val v)
end

let i_line_break v = Line_break_v begin match v with
| "AI" -> `AI | "AL" -> `AL | "B2" -> `B2 | "BA" -> `BA | "BB" -> `BB 
| "BK" -> `BK | "CB" -> `CB | "CJ" -> `CJ | "CL" -> `CL | "CM" -> `CM 
| "CP" -> `CP | "CR" -> `CR | "EX" -> `EX | "GL" -> `GL | "H2" -> `H2 
| "H3" -> `H3 | "HL" -> `HL | "HY" -> `HY | "ID" -> `ID | "IN" -> `IN 
| "IS" -> `IS | "JL" -> `JL | "JT" -> `JT | "JV" -> `JV | "LF" -> `LF 
| "NL" -> `NL | "NS" -> `NS | "NU" -> `NU | "OP" -> `OP | "PO" -> `PO 
| "PR" -> `PR | "QU" -> `QU | "SA" -> `SA | "SG" -> `SG | "SP" -> `SP 
| "SY" -> `SY | "WJ" -> `WJ | "XX" -> `XX | "ZW" -> `ZW 
| v -> err (err_att_val v)
end

let i_name v = Name_v (if String.contains v '#' then `Pattern v else `Name v)
let i_name_alias_type = function 
| "abbreviation" -> `Abbreviation | "alternate" -> `Alternate
| "control" -> `Control | "correction" -> `Correction | "figment" -> `Figment 
| v -> err (err_att_val v)
    
let i_numeric_type v = Numeric_type_v begin match v with
| "None" -> `None | "De" -> `De | "Di" -> `Di | "Nu" -> `Nu 
| v -> err (err_att_val v)
end
  
let i_numeric_value v = Numeric_value_v begin 
  try
    match (split_string v '/') with
    | ["NaN"] -> `NaN
    | [num; denom;] -> `Frac (int_of_string num, int_of_string denom)
    | [num ] -> `Num (Int64.of_string num)
    | _ -> failwith ""
  with Failure _ -> err (err_att_val v)
end

let i_script v = Script_v begin match v with
| "Arab" -> `Arab | "Armi" -> `Armi | "Armn" -> `Armn | "Avst" -> `Avst
| "Bali" -> `Bali | "Bamu" -> `Bamu | "Batk" -> `Batk | "Beng" -> `Beng 
| "Bopo" -> `Bopo | "Brah" -> `Brah | "Brai" -> `Brai | "Bugi" -> `Bugi 
| "Buhd" -> `Buhd | "Cakm" -> `Cakm | "Cans" -> `Cans | "Cari" -> `Cari 
| "Cham" -> `Cham | "Cher" -> `Cher | "Copt" -> `Copt | "Cprt" -> `Cprt 
| "Cyrl" -> `Cyrl | "Deva" -> `Deva | "Dsrt" -> `Dsrt | "Egyp" -> `Egyp 
| "Ethi" -> `Ethi | "Geor" -> `Geor | "Glag" -> `Glag | "Goth" -> `Goth 
| "Grek" -> `Grek | "Gujr" -> `Gujr | "Guru" -> `Guru | "Hang" -> `Hang 
| "Hani" -> `Hani | "Hano" -> `Hano | "Hebr" -> `Hebr | "Hira" -> `Hira 
| "Hrkt" -> `Hrkt | "Ital" -> `Ital | "Java" -> `Java | "Kali" -> `Kali 
| "Kana" -> `Kana | "Khar" -> `Khar | "Khmr" -> `Khmr | "Knda" -> `Knda 
| "Kthi" -> `Kthi | "Lana" -> `Lana | "Laoo" -> `Laoo | "Latn" -> `Latn 
| "Lepc" -> `Lepc | "Limb" -> `Limb | "Linb" -> `Linb | "Lisu" -> `Lisu 
| "Lyci" -> `Lyci | "Lydi" -> `Lydi | "Mand" -> `Mand | "Merc" -> `Merc 
| "Mero" -> `Mero | "Mlym" -> `Mlym | "Mong" -> `Mong | "Mtei" -> `Mtei 
| "Mymr" -> `Mymr | "Nkoo" -> `Nkoo | "Ogam" -> `Ogam | "Olck" -> `Olck 
| "Orkh" -> `Orkh | "Orya" -> `Orya | "Osma" -> `Osma | "Phag" -> `Phag 
| "Phli" -> `Phli | "Phnx" -> `Phnx | "Plrd" -> `Plrd | "Prti" -> `Prti
| "Qaai" -> `Qaai | "Rjng" -> `Rjng | "Runr" -> `Runr | "Samr" -> `Samr 
| "Sarb" -> `Sarb | "Saur" -> `Saur | "Shaw" -> `Shaw | "Shrd" -> `Shrd 
| "Sinh" -> `Sinh | "Sora" -> `Sora | "Sund" -> `Sund | "Sylo" -> `Sylo 
| "Syrc" -> `Syrc | "Tagb" -> `Tagb | "Takr" -> `Takr | "Tale" -> `Tale 
| "Talu" -> `Talu | "Taml" -> `Taml | "Tavt" -> `Tavt | "Telu" -> `Telu 
| "Tfng" -> `Tfng | "Tglg" -> `Tglg | "Thaa" -> `Thaa | "Thai" -> `Thai 
| "Tibt" -> `Tibt | "Ugar" -> `Ugar | "Vaii" -> `Vaii | "Xpeo" -> `Xpeo 
| "Xsux" -> `Xsux | "Yiii" -> `Yiii | "Zinh" -> `Zinh | "Zyyy" -> `Zyyy 
| "Zzzz" -> `Zzzz
| v -> err (err_att_val v)
end
  
let i_script_seq v = 
  let script v = o_script (i_script v) in
  Script_extensions_v (List.map script (split_string v ' '))

let i_sentence_break v = Sentence_break_v begin match v with 
| "AT" -> `AT | "CL" -> `CL | "CR" -> `CR | "EX" -> `EX | "FO" -> `FO 
| "LE" -> `LE | "LF" -> `LF | "LO" -> `LO | "NU" -> `NU | "SC" -> `SC 
| "SE" -> `SE | "SP" -> `SP | "ST" -> `ST | "UP" -> `UP | "XX" -> `XX 
| v -> err (err_att_val v)
end

let i_string v = String_v v
let i_uax_42_element v = UAX_42_element_v begin match v with 
| "reserved" -> `Reserved 
| "noncharacter" -> `Noncharacter
| "surrogate" -> `Surrogate
| "char" -> `Char
| s -> err (err_att_val s)
end

let i_word_break v = Word_break_v begin match v with
| "CR" -> `CR | "EX" -> `EX | "Extend" -> `Extend | "FO" -> `FO 
| "KA" -> `KA | "LE" -> `LE | "LF" -> `LF | "MB" -> `MB | "ML" -> `ML 
| "MN" -> `MN | "NL" -> `NL | "NU" -> `NU 
| "XX" -> `XX
| v -> err (err_att_val v)
end

module Pkey = struct type t = key let compare : key -> key -> int = compare end
module Pmap = Map.Make (Pkey)
type props = value Pmap.t 
type 'a prop = key * (value -> 'a)     (* property key and value projection. *)

let find props (k, o) = try Some (o (Pmap.find k props)) with Not_found -> None
let unknown_prop name = (Other name), o_string


(* non hunihan properties *)

let uax_42_element = UAX_42_element, o_uax_42_element    (* artefact of Uucd *)

let age = Age, o_age
let alphabetic = Alphabetic, o_bool
let ascii_hex_digit = Ascii_hex_digit, o_bool
let bidi_class = Bidi_class, o_bidi_class
let bidi_control = Bidi_control, o_bool
let bidi_mirrored = Bidi_mirrored, o_bool
let bidi_mirroring_glyph = Bidi_mirroring_glyph, o_cp_opt
let block = Block, o_block
let canonical_combining_class = Canonical_combining_class, o_int
let cased = Cased, o_bool
let case_folding = Case_folding, o_cps_map
let case_ignorable = Case_ignorable, o_bool
let changes_when_casefolded = Changes_when_casefolded, o_bool
let changes_when_casemapped = Changes_when_casemapped, o_bool
let changes_when_lowercased = Changes_when_lowercased, o_bool
let changes_when_nfkc_casefolded = Changes_when_nfkc_casefolded, o_bool
let changes_when_titlecased = Changes_when_titlecased, o_bool
let changes_when_uppercased = Changes_when_uppercased, o_bool
let composition_exclusion = Composition_exclusion, o_bool
let dash = Dash, o_bool
let decomposition_mapping = Decomposition_mapping, o_cps_map
let decomposition_type = Decomposition_type, o_decomposition_type
let default_ignorable_code_point = Default_ignorable_code_point, o_bool
let deprecated = Deprecated, o_bool
let diacritic = Diacritic, o_bool
let east_asian_width = East_asian_width, o_east_asian_width
let expands_on_nfc = Expands_on_nfc, o_bool
let expands_on_nfd = Expands_on_nfd, o_bool
let expands_on_nfkc = Expands_on_nfkc, o_bool
let expands_on_nfkd = Expands_on_nfkd, o_bool
let extender = Extender, o_bool
let fc_nfkc_closure = Fc_nfkc_closure, o_cps_map
let full_composition_exclusion = Full_composition_exclusion, o_bool
let general_category = General_category, o_general_category
let grapheme_base = Grapheme_base, o_bool
let grapheme_cluster_break = Grapheme_cluster_break, o_grapheme_cluster_break
let grapheme_extend = Grapheme_extend, o_bool
let grapheme_link = Grapheme_link, o_bool
let hangul_syllable_type = Hangul_syllable_type, o_hangul_syllable_type
let hex_digit = Hex_digit, o_bool
let hyphen = Hyphen, o_bool
let id_continue = Id_continue, o_bool
let id_start = Id_start, o_bool
let ideographic = Ideographic, o_bool
let ids_binary_operator = Ids_binary_operator, o_bool
let ids_trinary_operator = Ids_trinary_operator, o_bool
let indic_syllabic_category = Indic_syllabic_category, o_indic_syllabic_category
let indic_matra_category = Indic_matra_category, o_indic_matra_category
let iso_comment = Iso_comment, o_string
let jamo_short_name = Jamo_short_name, o_string
let join_control = Join_control, o_bool
let joining_group = Joining_group, o_joining_group
let joining_type = Joining_type, o_joining_type
let line_break = Line_break, o_line_break
let logical_order_exception = Logical_order_exception, o_bool
let lowercase = Lowercase, o_bool
let lowercase_mapping = Lowercase_mapping, o_cps_map
let math = Math, o_bool
let name = Name, o_name
let name_alias = Name_alias, o_name_alias
let nfc_quick_check = Nfc_quick_check, o_bool_maybe
let nfd_quick_check = Nfd_quick_check, o_bool_maybe
let nfkc_quick_check = Nfkc_quick_check, o_bool_maybe
let nfkc_casefold = Nfkc_casefold, o_cps_map
let nfkd_quick_check = Nfkd_quick_check, o_bool_maybe
let noncharacter_code_point = Noncharacter_code_point, o_bool
let numeric_type = Numeric_type, o_numeric_type
let numeric_value = Numeric_value, o_numeric_value
let other_alphabetic = Other_alphabetic, o_bool
let other_default_ignorable_code_point = 
  Other_default_ignorable_code_point, o_bool

let other_grapheme_extend = Other_grapheme_extend, o_bool
let other_id_continue = Other_id_continue, o_bool
let other_id_start = Other_id_start, o_bool
let other_lowercase = Other_lowercase, o_bool
let other_math = Other_math, o_bool
let other_uppercase = Other_uppercase, o_bool
let pattern_syntax = Pattern_syntax, o_bool
let pattern_white_space = Pattern_white_space, o_bool
let quotation_mark = Quotation_mark, o_bool
let radical = Radical, o_bool
let script = Script, o_script
let script_extensions = Script_extensions, o_script_extensions
let sentence_break = Sentence_break, o_sentence_break
let simple_case_folding = Simple_case_folding, o_cp_map
let simple_lowercase_mapping = Simple_lowercase_mapping, o_cp_map 
let simple_titlecase_mapping = Simple_titlecase_mapping, o_cp_map
let simple_uppercase_mapping = Simple_uppercase_mapping, o_cp_map
let soft_dotted = Soft_dotted, o_bool
let sterm = Sterm, o_bool
let terminal_punctuation = Terminal_punctuation, o_bool
let titlecase_mapping = Titlecase_mapping, o_cps_map
let unicode_1_name = Unicode_1_name, o_string
let unified_ideograph = Unified_ideograph, o_bool
let uppercase = Uppercase, o_bool
let uppercase_mapping = Uppercase_mapping, o_cps_map
let variation_selector = Variation_selector, o_bool
let white_space = White_space, o_bool
let word_break = Word_break, o_word_break
let xid_continue = Xid_continue, o_bool
let xid_start = Xid_start, o_bool

(* unihan properties *)

let kAccountingNumeric = KAccountingNumeric, o_string
let kAlternateHanYu = KAlternateHanYu, o_string
let kAlternateJEF = KAlternateJEF, o_string
let kAlternateKangXi = KAlternateKangXi, o_string
let kAlternateMorohashi = KAlternateMorohashi, o_string
let kBigFive = KBigFive, o_string
let kCCCII = KCCCII, o_string
let kCNS1986 = KCNS1986, o_string
let kCNS1992 = KCNS1992, o_string
let kCangjie = KCangjie, o_string
let kCantonese = KCantonese, o_string
let kCheungBauer = KCheungBauer, o_string
let kCheungBauerIndex = KCheungBauerIndex, o_string
let kCihaiT = KCihaiT, o_string
let kCompatibilityVariant = KCompatibilityVariant, o_string
let kCowles = KCowles, o_string
let kDaeJaweon = KDaeJaweon, o_string
let kDefinition = KDefinition, o_string
let kEACC = KEACC, o_string
let kFenn = KFenn, o_string
let kFennIndex = KFennIndex, o_string
let kFourCornerCode = KFourCornerCode, o_string
let kFrequency = KFrequency, o_string
let kGB0 = KGB0, o_string
let kGB1 = KGB1, o_string
let kGB3 = KGB3, o_string
let kGB5 = KGB5, o_string
let kGB7 = KGB7, o_string
let kGB8 = KGB8, o_string
let kGradeLevel = KGradeLevel, o_string
let kGSR = KGSR, o_string
let kHangul = KHangul, o_string
let kHanYu = KHanYu, o_string
let kHanyuPinlu = KHanyuPinlu, o_string
let kHanyuPinyin = KHanyuPinyin, o_string
let kHDZRadBreak = KHDZRadBreak, o_string
let kHKGlyph = KHKGlyph, o_string
let kHKSCS = KHKSCS, o_string
let kIBMJapan = KIBMJapan, o_string
let kIICore = KIICore, o_string
let kIRGDaeJaweon = KIRGDaeJaweon, o_string
let kIRGDaiKanwaZiten = KIRGDaiKanwaZiten, o_string
let kIRGHanyuDaZidian = KIRGHanyuDaZidian, o_string
let kIRGKangXi = KIRGKangXi, o_string
let kIRG_GSource = KIRG_GSource, o_string
let kIRG_HSource = KIRG_HSource, o_string
let kIRG_JSource = KIRG_JSource, o_string
let kIRG_KPSource = KIRG_KPSource, o_string
let kIRG_KSource = KIRG_KSource, o_string
let kIRG_MSource = KIRG_MSource, o_string
let kIRG_TSource = KIRG_TSource, o_string
let kIRG_USource = KIRG_USource, o_string
let kIRG_VSource = KIRG_VSource, o_string
let kJHJ = KJHJ, o_string
let kJIS0213 = KJIS0213, o_string
let kJapaneseKun = KJapaneseKun, o_string
let kJapaneseOn = KJapaneseOn, o_string
let kJis0 = KJis0, o_string
let kJis1 = KJis1, o_string
let kKPS0 = KKPS0, o_string
let kKPS1 = KKPS1, o_string
let kKSC0 = KKSC0, o_string
let kKSC1 = KKSC1, o_string
let kKangXi = KKangXi, o_string
let kKarlgren = KKarlgren, o_string
let kKorean = KKorean, o_string
let kLau = KLau, o_string
let kMainlandTelegraph = KMainlandTelegraph, o_string
let kMandarin = KMandarin, o_string
let kMatthews = KMatthews, o_string
let kMeyerWempe = KMeyerWempe, o_string
let kMorohashi = KMorohashi, o_string
let kNelson = KNelson, o_string
let kOtherNumeric = KOtherNumeric, o_string
let kPhonetic = KPhonetic, o_string
let kPrimaryNumeric = KPrimaryNumeric, o_string
let kPseudoGB1 = KPseudoGB1, o_string
let kRSAdobe_Japan1_6 = KRSAdobe_Japan1_6, o_string
let kRSJapanese = KRSJapanese, o_string
let kRSKanWa = KRSKanWa, o_string
let kRSKangXi = KRSKangXi, o_string
let kRSKorean = KRSKorean, o_string
let kRSMerged = KRSMerged, o_string
let kRSUnicode = KRSUnicode, o_string
let kSBGY = KSBGY, o_string
let kSemanticVariant = KSemanticVariant, o_string
let kSimplifiedVariant = KSimplifiedVariant, o_string
let kSpecializedSemanticVariant = KSpecializedSemanticVariant, o_string
let kTaiwanTelegraph = KTaiwanTelegraph, o_string
let kTang = KTang, o_string
let kTotalStrokes = KTotalStrokes, o_string
let kTraditionalVariant = KTraditionalVariant, o_string
let kVietnamese = KVietnamese, o_string
let kXHC1983 = KXHC1983, o_string
let kWubi = KWubi, o_string
let kXerox = KXerox, o_string
let kZVariant = KZVariant, o_string

(* Unicode Character Databases *)

type block = (cp * cp) * string
type named_sequence = string * cp list 
type normalization_correction = cp * cp list * cp list * (int * int * int)
type standardized_variant =
  cp list * string * [ `Isolate | `Initial | `Medial | `Final ] list

type cjk_radical = string * cp * cp 
type emoji_source = cp list * int option * int option * int option

type t = 
  { description : string; 
    repertoire : props Cpmap.t; 
    blocks : block list;
    named_sequences : named_sequence list;
    provisional_named_sequences : named_sequence list;
    normalization_corrections : normalization_correction list;
    standardized_variants : standardized_variant list; 
    cjk_radicals : cjk_radical list; 
    emoji_sources : emoji_source list; }

let cp_props db cp = 
  try Some (Cpmap.find cp db.repertoire) with Not_found -> None

let cp_prop db cp p = try find (Cpmap.find cp db.repertoire) p 
with Not_found -> None
    
(* Decode *)

(* Xml names *)

let ns_ucd = "http://www.unicode.org/ns/2003/ucd/1.0"
let n_block = (ns_ucd, "block")
let n_blocks = (ns_ucd, "blocks")
let n_char = (ns_ucd, "char")
let n_cjk_radical = (ns_ucd, "cjk-radical")
let n_cjk_radicals = (ns_ucd, "cjk-radicals")
let n_description = (ns_ucd, "description")
let n_emoji_source = (ns_ucd, "emoji-source")
let n_emoji_sources = (ns_ucd, "emoji-sources")
let n_group = (ns_ucd, "group")
let n_name_alias = (ns_ucd, "name-alias")
let n_named_sequence = (ns_ucd, "named-sequence")
let n_named_sequences = (ns_ucd, "named-sequences")
let n_noncharacter = (ns_ucd, "noncharacter")
let n_normalization_correction = (ns_ucd, "normalization-correction")
let n_normalization_corrections = (ns_ucd, "normalization-corrections")
let n_provisional_named_sequences = (ns_ucd, "provisional-named-sequences")
let n_repertoire = (ns_ucd, "repertoire")
let n_reserved = (ns_ucd, "reserved")
let n_standardized_variant = (ns_ucd, "standardized-variant")
let n_standardized_variants = (ns_ucd, "standardized-variants")
let n_surrogate = (ns_ucd, "surrogate")
let n_ucd = (ns_ucd, "ucd")

(* Attribute parsing *)
      
let add_prop : value Pmap.t -> Xmlm.attribute -> value Pmap.t =        
  let h = Hashtbl.create 500 in 
  let map = Hashtbl.add h in
  map "AHex" (Ascii_hex_digit, i_bool);
  map "Alpha" (Alphabetic, i_bool);
  map "Bidi_C" (Bidi_control, i_bool);
  map "Bidi_M" (Bidi_mirrored, i_bool);
  map "Cased" (Cased, i_bool);
  map "CI" (Case_ignorable, i_bool);
  map "CE" (Composition_exclusion, i_bool);
  map "CWCF" (Changes_when_casefolded, i_bool);
  map "CWCM" (Changes_when_casemapped, i_bool);
  map "CWL" (Changes_when_lowercased, i_bool); 
  map "CWKCF" (Changes_when_nfkc_casefolded, i_bool);
  map "CWT" (Changes_when_titlecased, i_bool);
  map "CWU" (Changes_when_uppercased, i_bool);
  map "Comp_Ex" (Full_composition_exclusion, i_bool);
  map "DI" (Default_ignorable_code_point, i_bool);
  map "Dash" (Dash, i_bool);
  map "Dep" (Deprecated, i_bool);
  map "Dia" (Diacritic, i_bool);
  map "Ext" (Extender, i_bool);
  map "FC_NFKC" (Fc_nfkc_closure, i_cps_map ~empty:false);
  map "GCB" (Grapheme_cluster_break, i_grapheme_cluster_break);
  map "Gr_Base" (Grapheme_base, i_bool);
  map "Gr_Ext" (Grapheme_extend, i_bool);
  map "Gr_Link" (Grapheme_link, i_bool);
  map "Hex" (Hex_digit, i_bool);
  map "Hyphen" (Hyphen, i_bool);
  map "IDC" (Id_continue, i_bool);
  map "IDS" (Id_start, i_bool);
  map "IDSB" (Ids_binary_operator, i_bool);
  map "IDST" (Ids_trinary_operator, i_bool);
  map "Ideo" (Ideographic, i_bool);
  map "InSC" (Indic_syllabic_category, i_indic_syllabic_category);
  map "InMC" (Indic_matra_category, i_indic_matra_category);
  map "JSN" (Jamo_short_name, i_string);
  map "Join_C" (Join_control, i_bool);
  map "LOE" (Logical_order_exception, i_bool);
  map "Lower" (Lowercase, i_bool);
  map "Math" (Math, i_bool);
  map "NChar" (Noncharacter_code_point, i_bool);
  map "NFC_QC" (Nfc_quick_check, i_bool_maybe);
  map "NFD_QC" (Nfd_quick_check, i_bool_maybe);
  map "NFKC_QC" (Nfkc_quick_check, i_bool_maybe);
  map "NFKC_CF" (Nfkc_casefold, i_cps_map ~empty:true);
  map "NFKD_QC" (Nfkd_quick_check, i_bool_maybe);
  map "OAlpha" (Other_alphabetic, i_bool);
  map "ODI" (Other_default_ignorable_code_point, i_bool);
  map "OGr_Ext" (Other_grapheme_extend, i_bool);
  map "OIDC" (Other_id_continue, i_bool);
  map "OIDS" (Other_id_start, i_bool);
  map "OLower" (Other_lowercase, i_bool);
  map "OMath" (Other_math, i_bool);
  map "OUpper" (Other_uppercase, i_bool);
  map "Pat_Syn" (Pattern_syntax, i_bool);
  map "Pat_WS" (Pattern_white_space, i_bool);
  map "QMark" (Quotation_mark, i_bool);
  map "Radical" (Radical, i_bool);
  map "SB" (Sentence_break, i_sentence_break);
  map "SD" (Soft_dotted, i_bool);
  map "STerm" (Sterm, i_bool);
  map "Term" (Terminal_punctuation, i_bool);
  map "UIdeo" (Unified_ideograph, i_bool);
  map "Upper" (Uppercase, i_bool);
  map "VS" (Variation_selector, i_bool);
  map "WB" (Word_break, i_word_break);
  map "WSpace" (White_space, i_bool);
  map "XIDC" (Xid_continue, i_bool);
  map "XIDS" (Xid_start, i_bool);
  map "XO_NFC" (Expands_on_nfc, i_bool);
  map "XO_NFD" (Expands_on_nfd, i_bool);
  map "XO_NFKC" (Expands_on_nfkc, i_bool);
  map "XO_NFKD" (Expands_on_nfkd, i_bool);
  map "age" (Age, i_age);
  map "bc" (Bidi_class, i_bidi_class);
  map "blk" (Block, i_block);
  map "bmg" (Bidi_mirroring_glyph, i_cp_opt);
  map "ccc" (Canonical_combining_class, i_int);
  map "cf" (Case_folding, i_cps_map ~empty:false);
  map "dm" (Decomposition_mapping,  (i_cps_map ~empty:true));
  map "dt" (Decomposition_type, i_decomposition_type);
  map "ea" (East_asian_width, i_east_asian_width);
  map "gc" (General_category, i_general_category);
  map "hst" (Hangul_syllable_type, i_hangul_syllable_type);
  map "isc" (Iso_comment, i_string);
  map "jg" (Joining_group, i_joining_group);
  map "jt" (Joining_type, i_joining_type);
  map "lb" (Line_break, i_line_break);
  map "lc" (Lowercase_mapping, i_cps_map ~empty:false);
  map "na" (Name, i_name);
  map "na1" (Unicode_1_name, i_string);
  map "nt" (Numeric_type, i_numeric_type);
  map "nv" (Numeric_value, i_numeric_value);
  map "sc" (Script, i_script);
  map "scf" (Simple_case_folding, i_cp_map);
  map "scx" (Script_extensions, i_script_seq);
  map "slc" (Simple_lowercase_mapping, i_cp_map);
  map "stc" (Simple_titlecase_mapping, i_cp_map);
  map "suc" (Simple_uppercase_mapping, i_cp_map);
  map "tc" (Titlecase_mapping, i_cps_map ~empty:false);
  map "uax_42_element" (UAX_42_element, i_uax_42_element);       (* artefact *)
  map "uc" (Uppercase_mapping, i_cps_map ~empty:false);
  map "kAccountingNumeric" (KAccountingNumeric, i_string);
  map "kAlternateHanYu" (KAlternateHanYu, i_string);
  map "kAlternateJEF" (KAlternateJEF, i_string);
  map "kAlternateKangXi" (KAlternateKangXi, i_string);
  map "kAlternateMorohashi" (KAlternateMorohashi, i_string);
  map "kBigFive" (KBigFive, i_string);
  map "kCCCII" (KCCCII, i_string);
  map "kCNS1986" (KCNS1986, i_string);
  map "kCNS1992" (KCNS1992, i_string);
  map "kCangjie" (KCangjie, i_string);
  map "kCantonese" (KCantonese, i_string);
  map "kCheungBauer" (KCheungBauer, i_string);
  map "kCheungBauerIndex" (KCheungBauerIndex, i_string);
  map "kCihaiT" (KCihaiT, i_string);
  map "kCompatibilityVariant" (KCompatibilityVariant, i_string);
  map "kCowles" (KCowles, i_string);
  map "kDaeJaweon" (KDaeJaweon, i_string);
  map "kDefinition" (KDefinition, i_string);
  map "kEACC" (KEACC, i_string);
  map "kFenn" (KFenn, i_string);
  map "kFennIndex" (KFennIndex, i_string);
  map "kFourCornerCode" (KFourCornerCode, i_string);
  map "kFrequency" (KFrequency, i_string);
  map "kGB0" (KGB0, i_string);
  map "kGB1" (KGB1, i_string);
  map "kGB3" (KGB3, i_string);
  map "kGB5" (KGB5, i_string);
  map "kGB7" (KGB7, i_string);
  map "kGB8" (KGB8, i_string);
  map "kGradeLevel" (KGradeLevel, i_string);
  map "kGSR" (KGSR, i_string);
  map "kHangul" (KHangul, i_string);
  map "kHanYu" (KHanYu, i_string);
  map "kHanyuPinlu" (KHanyuPinlu, i_string);
  map "kHanyuPinyin" (KHanyuPinyin, i_string);
  map "kHDZRadBreak" (KHDZRadBreak, i_string);
  map "kHKGlyph" (KHKGlyph, i_string);
  map "kHKSCS" (KHKSCS, i_string);
  map "kIBMJapan" (KIBMJapan, i_string);
  map "kIICore" (KIICore, i_string);
  map "kIRGDaeJaweon" (KIRGDaeJaweon, i_string);
  map "kIRGDaiKanwaZiten" (KIRGDaiKanwaZiten, i_string);
  map "kIRGHanyuDaZidian" (KIRGHanyuDaZidian, i_string);
  map "kIRGKangXi" (KIRGKangXi, i_string);
  map "kIRG_GSource" (KIRG_GSource, i_string);
  map "kIRG_HSource" (KIRG_HSource, i_string);
  map "kIRG_JSource" (KIRG_JSource, i_string);
  map "kIRG_KPSource" (KIRG_KPSource, i_string);
  map "kIRG_KSource" (KIRG_KSource, i_string);
  map "kIRG_MSource" (KIRG_MSource, i_string);
  map "kIRG_TSource" (KIRG_TSource, i_string);
  map "kIRG_USource" (KIRG_USource, i_string);
  map "kIRG_VSource" (KIRG_VSource, i_string);
  map "kJHJ" (KJHJ, i_string);
  map "kJIS0213" (KJIS0213, i_string);
  map "kJapaneseKun" (KJapaneseKun, i_string);
  map "kJapaneseOn" (KJapaneseOn, i_string);
  map "kJis0" (KJis0, i_string);
  map "kJis1" (KJis1, i_string);
  map "kKPS0" (KKPS0, i_string);
  map "kKPS1" (KKPS1, i_string);
  map "kKSC0" (KKSC0, i_string);
  map "kKSC1" (KKSC1, i_string);
  map "kKangXi" (KKangXi, i_string);
  map "kKarlgren" (KKarlgren, i_string);
  map "kKorean" (KKorean, i_string);
  map "kLau" (KLau, i_string);
  map "kMainlandTelegraph" (KMainlandTelegraph, i_string);
  map "kMandarin" (KMandarin, i_string);
  map "kMatthews" (KMatthews, i_string);
  map "kMeyerWempe" (KMeyerWempe, i_string);
  map "kMorohashi" (KMorohashi, i_string);
  map "kNelson" (KNelson, i_string);
  map "kOtherNumeric" (KOtherNumeric, i_string);
  map "kPhonetic" (KPhonetic, i_string);
  map "kPrimaryNumeric" (KPrimaryNumeric, i_string);
  map "kPseudoGB1" (KPseudoGB1, i_string);
  map "kRSAdobe_Japan1_6" (KRSAdobe_Japan1_6, i_string);
  map "kRSJapanese" (KRSJapanese, i_string);
  map "kRSKanWa" (KRSKanWa, i_string);
  map "kRSKangXi" (KRSKangXi, i_string);
  map "kRSKorean" (KRSKorean, i_string);
  map "kRSMerged" (KRSMerged, i_string);
  map "kRSUnicode" (KRSUnicode, i_string);
  map "kSBGY" (KSBGY, i_string);
  map "kSemanticVariant" (KSemanticVariant, i_string);
  map "kSimplifiedVariant" (KSimplifiedVariant, i_string);
  map "kSpecializedSemanticVariant" (KSpecializedSemanticVariant, i_string);
  map "kTaiwanTelegraph" (KTaiwanTelegraph, i_string);
  map "kTang" (KTang, i_string);
  map "kTotalStrokes" (KTotalStrokes, i_string);
  map "kTraditionalVariant" (KTraditionalVariant, i_string);
  map "kVietnamese" (KVietnamese, i_string);
  map "kXHC1983" (KXHC1983, i_string);
  map "kWubi" (KWubi, i_string);
  map "kXerox" (KXerox, i_string);
  map "kZVariant" (KZVariant, i_string);
  fun m (n, v) -> 
    try match n with
    | ("", p) -> 
        let k, conv = Hashtbl.find h p in
        Pmap.add k (conv v) m
    | _ -> raise Not_found
    with Not_found -> Pmap.add (Other n) (i_string v) m

let attv n atts =               (* value of attribute [n] in atts or raises. *)
  try snd (List.find (fun (en, v) -> en = ("", n)) atts) with
  | Not_found -> err_miss_att n
      
let rec skip_el d =             (* skips an element, start signal was input. *)
  let rec loop d depth = match Xmlm.input d with
  | `El_start _ -> loop d (depth + 1)
  | `El_end -> if depth = 0 then () else loop d (depth - 1)
  | s -> loop d depth
  in
  loop d 0 

(* Parses a sequence of empty elements named n and a El_end. *)
let p_seq n p_atts d = 
  let rec aux n p_atts d acc = match Xmlm.input d with 
  | `El_start (n', atts) when n' = n ->
      if Xmlm.input d <> `El_end then err err_exp_el_end else 
      aux n p_atts d ((p_atts atts) :: acc);
  | `El_start _ -> skip_el d; aux n p_atts d acc
  | `El_end -> List.rev acc
  | `Data _ -> err err_data
  | _ -> assert false
  in
  aux n p_atts d []
  
let p_description d = match (Xmlm.input d) with
| `Data desc -> if (Xmlm.input d <> `El_end) then err err_exp_el_end else desc
| `El_end -> ""
| _ -> err err_exp_data 

let p_name_aliases d =
  let rec loop d depth acc = match Xmlm.peek d with 
  | `El_start (n, atts) when n = n_name_alias -> 
      ignore (Xmlm.input d);
      let alias = ref "" in 
      let atype = ref None in
      let p_alias_atts = function 
      | ("", "alias"), v -> alias := v
      | ("", "type"), v -> atype := Some (i_name_alias_type v)
      | _ -> ()
      in
      List.iter p_alias_atts atts;
      begin match !atype with None -> err err_invalid_name_alias_spec
      | Some t -> loop d (depth + 1) ((!alias, t) :: acc)
      end
  | `El_start (n, atts) -> ignore (Xmlm.input d); skip_el d; loop d depth acc
  | `El_end ->
      if depth = 0 then List.rev acc else 
      (ignore (Xmlm.input d); loop d (depth - 1) acc)
  | `Data _ -> err err_data
  | _ -> assert false
  in
  loop d 0 [] 

let p_cp d rep atts g_props =
  let cp = ref None in 
  let cp_first = ref None in
  let cp_last = ref None in
  let add acc ((n, v) as a) = match n with
  | ("", "cp") -> cp := Some (cp_of_string v); acc
  | ("", "first-cp") -> cp_first := Some (cp_of_string v); acc 
  | ("", "last-cp") -> cp_last := Some (cp_of_string v); acc
  | _ -> add_prop acc a
  in
  let props = List.fold_left add g_props atts in
  let props = Pmap.add Name_alias (Name_alias_v (p_name_aliases d)) props in
  match !cp with
  | Some cp -> Cpmap.add cp props rep
  | None -> match !cp_first, !cp_last with
    | Some f, Some l -> 
	let rep = ref rep in 
	for cp = f to l do rep := Cpmap.add cp props !rep done;
	!rep
    | _ -> err err_invalid_cp_spec
        
let p_repertoire d =
  let eatt t = ("","uax_42_element"), t in (* fake attribute for uniformity *)
  let rec loop d depth rep g_atts = match Xmlm.input d with 
  | `El_start (n, atts) when n = n_reserved -> 
      loop d (depth + 1) (p_cp d rep (eatt "reserved" :: atts) g_atts) g_atts
  | `El_start (n, atts) when n = n_noncharacter ->
      loop d (depth + 1) (p_cp d rep (eatt "noncharacter":: atts) g_atts) g_atts
  | `El_start (n, atts) when n = n_surrogate -> 
      loop d (depth + 1) (p_cp d rep (eatt "surrogate" :: atts) g_atts) g_atts
  | `El_start (n, atts) when n = n_char -> 
      loop d (depth + 1) (p_cp d rep (eatt "char" :: atts) g_atts) g_atts
  | `El_start (n, atts) when n = n_group ->    
      let atts = List.fold_left add_prop Pmap.empty atts in 
      let rep = loop d 0 rep atts in   (* ^ empty: no group hierarchy *)
      loop d depth rep Pmap.empty 
  | `El_start (n, atts) -> skip_el d; loop d depth rep g_atts (* skip foreign *)
  | `El_end -> if depth = 0 then rep else loop d (depth - 1) rep g_atts
  | `Data _ -> err err_data
  | _ -> assert false
  in
  loop d 0 Cpmap.empty Pmap.empty

let p_blocks d =
  let b_atts atts = 
    (cp_of_string (attv "first-cp" atts), cp_of_string (attv "last-cp" atts)),
    attv "name" atts
  in
  p_seq n_block b_atts d
    
let p_named_sequences d =
  let ns_atts atts = attv "name" atts, cps_of_string (attv "cps" atts) in
  p_seq n_named_sequence ns_atts d
  
let p_normalization_corrections d = 
  let version_of_string v = 
    try
      match List.map int_of_string (split_string v '.') with
      | [v1; v2; v3;] -> (v1, v2, v3)
      | _ -> failwith ""
    with Failure _ -> err (err_att_val v)
  in
  let nc_atts atts = 
    cp_of_string (attv "cp" atts),
    cps_of_string (attv "old" atts),
    cps_of_string (attv "new" atts),
    version_of_string (attv "version" atts)
  in
  p_seq n_normalization_correction nc_atts d

let p_standardized_variants d =
  let when_of_string v = 
    let w s = match s with
    | "isolate" -> `Isolate 
    | "initial" -> `Initial 
    | "medial" -> `Medial 
    | "final" -> `Final
    | s -> err (err_att_val s)
    in
    List.map w (split_string v ' ')
  in  
  let sv_atts atts = 
    cps_of_string (attv "cps" atts), 
    attv "desc" atts, 
    when_of_string (attv "when" atts)
  in
  p_seq n_standardized_variant sv_atts d

let p_cjk_radicals d = 
  let cjk_r_atts atts = 
    attv "number" atts, 
    cp_of_string (attv "radical" atts), 
    cp_of_string (attv "ideograph" atts)
  in
  p_seq n_cjk_radical cjk_r_atts d

let p_emoji_sources d = 
  let es_atts atts =
    let opt_int v = if v = "" then None else Some (cp_of_string v) in
    cps_of_string (attv "unicode" atts), 
    opt_int (attv "docomo" atts), 
    opt_int (attv "kddi" atts), 
    opt_int (attv "softbank" atts)
  in
  p_seq n_emoji_source es_atts d

let p_ucd d = 
  let description = ref None in 
  let repertoire = ref None in 
  let blocks = ref None in
  let named_sequences = ref None in
  let provisional_named_sequences = ref None in
  let normalization_corrections = ref None in
  let standardized_variants = ref None in 
  let cjk_radicals = ref None in
  let emoji_sources = ref None in
  let set n r p d = if !r <> None then err (err_dup n) else r := Some (p d) in
  while (Xmlm.peek d <> `El_end) do match Xmlm.input d with
  | `El_start (n, _) when n = n_description -> 
      set n description p_description d
  | `El_start (n, _) when n = n_repertoire -> 
      set n repertoire p_repertoire d 
  | `El_start (n, _) when n = n_blocks -> 
      set n blocks p_blocks d
  | `El_start (n, _) when n = n_named_sequences -> 
      set n named_sequences p_named_sequences d 
  | `El_start (n, _) when n = n_provisional_named_sequences ->
      set n provisional_named_sequences p_named_sequences d
  | `El_start (n, _) when n = n_normalization_corrections ->
      set n normalization_corrections p_normalization_corrections d 
  | `El_start (n, _) when n = n_standardized_variants ->
      set n standardized_variants p_standardized_variants d
  | `El_start (n, _) when n = n_cjk_radicals -> 
      set n cjk_radicals p_cjk_radicals d
  | `El_start (n, _) when n = n_emoji_sources -> 
      set n emoji_sources p_emoji_sources d
  | `El_start (n, _) -> skip_el d                        (* foreign markup *)
  | `Data _ -> err err_data
  | _ -> assert false
  done;
  ignore (Xmlm.input d);
  if not (Xmlm.eoi d) then err err_wf;
  let some v default = match v with Some v -> v | None -> default in
  { description = some !description "";
    repertoire = some !repertoire Cpmap.empty;
    blocks = some !blocks [];
    named_sequences = some !named_sequences [];
    provisional_named_sequences = some !provisional_named_sequences [];
    normalization_corrections = some !normalization_corrections [];
    standardized_variants = some !standardized_variants []; 
    cjk_radicals = some !cjk_radicals []; 
    emoji_sources = some !emoji_sources []; }

type src = [ `Channel of in_channel | `String of string ]
type decoder = Xmlm.input

let decoder src =
  let src = match src with `String s -> `String (0, s) | `Channel _ as s -> s in
  Xmlm.make_input ~strip:true src

let decoded_range d = Xmlm.pos d, Xmlm.pos d
let decode d = try
  ignore (Xmlm.input d); (* `Dtd *)
  begin match Xmlm.input d with
  | `El_start (n, _) when n = n_ucd -> `Ok (p_ucd d) 	
  | `El_start (n, _) -> err (err_exp_ucd n)
  | _ -> assert false
  end;
with
| Failure e -> `Error e | Xmlm.Error (_, e) -> `Error (Xmlm.error_message e)

(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
