(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Unicode character database decoder.

    [Uucd] decodes the data of the
    {{:http://www.unicode.org/reports/tr44}Unicode character database}
    from its XML representation. It provides high-level (but not
    necessarily efficient) access to the data so that efficient
    representations can be extracted.

    [Uucd] decodes the representation described in the Annex #42 of
    Unicode %%UNICODE_VERSION%%. Subsequent versions may be decoded as
    long as no new cases are introduced in parsed enumerated
    properties.

    Consult the {{!basics}basics}.

    {b Note.} All strings returned by the module are UTF-8 encoded.

    {e Release %%VERSION%% — Unicode version %%UNICODE_VERSION%% —
       %%MAINTAINER%% }
    {3 References}
    {ul
    {- The Unicode Consortium.
    {e {{:http://www.unicode.org/versions/latest}The Unicode Standard}}.
    (latest version)}
    {- Mark Davis, Ken Whistler.
    {e {{:http://www.unicode.org/reports/tr44/}UAX #44 Unicode Character
    Database}}. (latest version)}
    {- Eric Muller.
    {e {{:http://www.unicode.org/reports/tr42/}UAX #42 Unicode Character
    Database in XML}}. (latest version)}} *)

(** {1:chars Code points} *)

type cp = int
(** The type for Unicode {{:http://unicode.org/glossary/#code_point}code
    points}, ranges from [0x0000] to [0x10_FFFF]. *)

val is_cp : int -> bool
(** [is_cp n] is [true] iff [n] a Unicode
    {{:http://unicode.org/glossary/#code_point}code
    point}. *)

val is_scalar_value : int -> bool
(** [is_scalar_value n] is [true] iff [n] is a Unicode
    {{:http://unicode.org/glossary/#Unicode_scalar_value}scalar value}. *)

(** Code point maps. *)
module Cpmap : Map.S with type key = cp

(** {1:props Properties}

    Properties are referenced by their name and property values by
    their
    {{:http://www.unicode.org/Public/UNIDATA/PropertyValueAliases.txt}
    abbreviated name}. To understand their semantics refer to the
    {{:http://www.unicode.org/versions/latest/}standard}. *)

type props
(** The type for sets of properties. *)

type 'a prop
(** The type for properties with property value of type ['a]. *)

val find : props -> 'a prop -> 'a option
(** [find ps p] is the value of property [p] in [ps], if any. *)

val unknown_prop : string * string -> string prop
(** [unknown_prop (ns, n)] is a property read from an XML attribute
    whose expanded name is [(ns, n)]. This can be used to access a
    property unknown to the module. *)

(** {2:nonunihan Non Unihan properties}

    In alphabetical order. *)

val age : [ `Version of int * int | `Unassigned ] prop
val alphabetic : bool prop
val ascii_hex_digit : bool prop
val bidi_class :
  [ `AL | `AN | `B | `BN | `CS | `EN | `ES | `ET | `L | `LRE | `LRO | `NSM
  | `ON | `PDF | `R | `RLE | `RLO | `S | `WS | `LRI | `RLI | `FSI | `PDI ] prop

val bidi_control : bool prop
val bidi_mirrored : bool prop
val bidi_mirroring_glyph : cp option prop
val bidi_paired_bracket : [ `Self | `Cp of cp ] prop
val bidi_paired_bracket_type : [ `O | `C | `N ] prop
val block :
[ `Aegean_Numbers | `Ahom | `Alchemical | `Alphabetic_PF
| `Anatolian_Hieroglyphs | `Ancient_Greek_Music
| `Ancient_Greek_Numbers | `Ancient_Symbols | `Arabic | `Arabic_Ext_A
| `Arabic_Math | `Arabic_PF_A | `Arabic_PF_B | `Arabic_Sup | `Armenian
| `Arrows | `ASCII | `Avestan | `Balinese | `Bamum | `Bamum_Sup | `Bassa_Vah
| `Batak | `Bengali | `Block_Elements | `Bopomofo | `Bopomofo_Ext
| `Box_Drawing | `Brahmi | `Braille | `Buginese | `Buhid | `Byzantine_Music
| `Carian | `Caucasian_Albanian | `Chakma | `Cham | `Cherokee | `Cherokee_Sup
| `CJK | `CJK_Compat | `CJK_Compat_Forms | `CJK_Compat_Ideographs
| `CJK_Compat_Ideographs_Sup | `CJK_Ext_A | `CJK_Ext_B | `CJK_Ext_C
| `CJK_Ext_D | `CJK_Ext_E | `CJK_Radicals_Sup | `CJK_Strokes | `CJK_Symbols
| `Compat_Jamo | `Control_Pictures | `Coptic | `Coptic_Epact_Numbers
| `Counting_Rod
| `Cuneiform | `Cuneiform_Numbers | `Currency_Symbols | `Cypriot_Syllabary
| `Cyrillic | `Cyrillic_Ext_A | `Cyrillic_Ext_B | `Cyrillic_Sup | `Deseret
| `Devanagari | `Devanagari_Ext | `Diacriticals | `Diacriticals_For_Symbols
| `Diacriticals_Sup | `Diacriticals_Ext | `Dingbats | `Domino | `Duployan
| `Early_Dynastic_Cuneiform | `Egyptian_Hieroglyphs | `Emoticons
| `Enclosed_Alphanum | `Enclosed_Alphanum_Sup | `Enclosed_CJK
| `Enclosed_Ideographic_Sup | `Ethiopic | `Ethiopic_Ext | `Ethiopic_Ext_A
| `Ethiopic_Sup | `Elbasan | `Geometric_Shapes | `Geometric_Shapes_Ext
| `Georgian | `Georgian_Sup | `Glagolitic | `Gothic | `Grantha | `Greek
| `Greek_Ext | `Gujarati | `Gurmukhi | `Half_And_Full_Forms | `Half_Marks
| `Hangul | `Hanunoo | `Hatran | `Hebrew | `High_PU_Surrogates
| `High_Surrogates | `Hiragana | `IDC | `Imperial_Aramaic
| `Indic_Number_Forms | `Inscriptional_Pahlavi | `Inscriptional_Parthian
| `IPA_Ext | `Jamo | `Jamo_Ext_A | `Jamo_Ext_B | `Javanese | `Kaithi
| `Kana_Sup | `Kanbun | `Kangxi | `Kannada | `Katakana | `Katakana_Ext
| `Kayah_Li | `Kharoshthi | `Khmer | `Khmer_Symbols | `Khojki | `Khudawadi
| `Lao | `Latin_1_Sup | `Latin_Ext_A | `Latin_Ext_Additional | `Latin_Ext_B
| `Latin_Ext_C | `Latin_Ext_D | `Latin_Ext_E | `Lepcha | `Letterlike_Symbols
| `Limbu | `Linear_A | `Linear_B_Ideograms
| `Linear_B_Syllabary | `Lisu | `Low_Surrogates | `Lycian | `Lydian
| `Mahajani | `Mahjong
| `Malayalam | `Mandaic | `Manichaean | `Math_Alphanum | `Math_Operators
| `Meetei_Mayek | `Meetei_Mayek_Ext | `Mende_Kikakui | `Meroitic_Cursive
| `Meroitic_Hieroglyphs | `Miao | `Misc_Arrows | `Misc_Math_Symbols_A
| `Misc_Math_Symbols_B | `Misc_Pictographs | `Misc_Symbols
| `Misc_Technical | `Modi | `Modifier_Letters
| `Modifier_Tone_Letters | `Mongolian | `Mro | `Music | `Multani | `Myanmar
| `Myanmar_Ext_A | `Myanmar_Ext_B | `Nabataean | `NB | `New_Tai_Lue | `NKo
| `Number_Forms | `OCR | `Ogham | `Ol_Chiki | `Old_Hungarian | `Old_Italic
| `Old_North_Arabian | `Old_Permic | `Old_Persian | `Old_South_Arabian
| `Old_Turkic | `Oriya
| `Ornamental_Dingbats | `Osmanya | `Pahawh_Hmong | `Palmyrene | `Pau_Cin_Hau
| `Phags_Pa | `Phaistos | `Phoenician
| `Phonetic_Ext | `Phonetic_Ext_Sup | `Playing_Cards | `Psalter_Pahlavi | `PUA
| `Punctuation | `Rejang | `Rumi | `Runic | `Samaritan | `Saurashtra
| `Sharada | `Shavian | `Shorthand_Format_Controls | `Siddham | `Sinhala
| `Sinhala_Archaic_Numbers | `Small_Forms | `Sora_Sompeng
| `Specials | `Sundanese | `Sundanese_Sup | `Sup_Arrows_A | `Sup_Arrows_B
| `Sup_Arrows_C | `Sup_Math_Operators | `Sup_PUA_A | `Sup_PUA_B
| `Sup_Punctuation | `Sup_Symbols_And_Pictographs | `Super_And_Sub
| `Sutton_SignWriting | `Syloti_Nagri | `Syriac
| `Tagalog | `Tagbanwa | `Tags | `Tai_Le | `Tai_Tham | `Tai_Viet
| `Tai_Xuan_Jing | `Takri | `Tamil | `Telugu | `Thaana | `Thai | `Tibetan
| `Tifinagh | `Tirhuta | `Transport_And_Map | `UCAS
| `UCAS_Ext | `Ugaritic | `Vai | `Vedic_Ext | `Vertical_Forms | `VS | `VS_Sup
| `Warang_Citi | `Yi_Radicals | `Yi_Syllables | `Yijing ] prop


val canonical_combining_class : int prop
val cased : bool prop
val case_folding : [`Self | `Cps of cp list ] prop
val case_ignorable : bool prop
val changes_when_casefolded : bool prop
val changes_when_casemapped : bool prop
val changes_when_lowercased : bool prop
val changes_when_nfkc_casefolded : bool prop
val changes_when_titlecased : bool prop
val changes_when_uppercased : bool prop
val composition_exclusion : bool prop
val dash : bool prop
val decomposition_mapping : [`Self | `Cps of cp list ] prop
val decomposition_type :
  [ `Can  | `Com | `Enc | `Fin  | `Font | `Fra | `Init | `Iso | `Med | `Nar
  | `Nb   | `Sml | `Sqr  | `Sub | `Sup | `Vert | `Wide | `None ] prop

val default_ignorable_code_point : bool prop
val deprecated : bool prop
val diacritic : bool prop
val east_asian_width : [ `A | `F | `H | `N | `Na | `W ] prop
val expands_on_nfc : bool prop
val expands_on_nfd : bool prop
val expands_on_nfkc : bool prop
val expands_on_nfkd : bool prop
val extender : bool prop
val fc_nfkc_closure : [ `Self | `Cps of cp list ] prop
val full_composition_exclusion : bool prop
val general_category :
  [ `Lu | `Ll | `Lt | `Lm | `Lo | `Mn | `Mc | `Me | `Nd | `Nl | `No
  | `Pc | `Pd | `Ps | `Pe | `Pi | `Pf | `Po | `Sm | `Sc | `Sk | `So
  | `Zs | `Zl | `Zp | `Cc | `Cf | `Cs | `Co | `Cn ] prop

val grapheme_base : bool prop
val grapheme_cluster_break :
  [ `CN | `CR | `EX | `L  | `LF |  `LV | `LVT | `PP | `RI | `SM | `T  | `V
  | `XX ] prop

val grapheme_extend : bool prop
val grapheme_link : bool prop
val hangul_syllable_type : [ `L | `LV | `LVT | `T | `V | `NA ] prop
val hex_digit : bool prop
val hyphen : bool prop
val id_continue : bool prop
val id_start : bool prop
val ideographic : bool prop
val ids_binary_operator : bool prop
val ids_trinary_operator : bool prop
val indic_syllabic_category :
  [ `Bindu
  | `Visarga
  | `Avagraha
  | `Nukta
  | `Virama
  | `Pure_Killer
  | `Invisible_Stacker
  | `Vowel_Independent
  | `Vowel_Dependent
  | `Vowel
  | `Consonant_Placeholder
  | `Consonant
  | `Consonant_Dead
  | `Consonant_Preceding_Repha
  | `Consonant_Succeeding_Repha
  | `Consonant_Repha
  | `Consonant_Subjoined
  | `Consonant_Medial
  | `Consonant_Final
  | `Consonant_Head_Letter
  | `Consonant_Killer
  | `Modifying_Letter
  | `Tone_Letter
  | `Tone_Mark
  | `Gemination_Mark
  | `Cantillation_Mark
  | `Register_Shifter
  | `Syllable_Modifier
  | `Non_Joiner
  | `Joiner
  | `Number_Joiner
  | `Number
  | `Brahmi_Joining_Number
  | `Other ] prop

val indic_matra_category :
  [ `Right | `Left | `Visual_Order_Left | `Left_And_Right | `Top | `Bottom
  | `Top_And_Bottom | `Top_And_Right | `Top_And_Left | `Top_And_Left_And_Right
  | `Bottom_And_Right | `Top_And_Bottom_And_Right | `Overstruck | `Invisible
  | `NA ] prop

val indic_positional_category :
 [ `Bottom
 | `Bottom_And_Right
 | `Left
 | `Left_And_Right
 | `NA
 | `Overstruck
 | `Right
 | `Top
 | `Top_And_Bottom
 | `Top_And_Bottom_And_Right
 | `Top_And_Left
 | `Top_And_Left_And_Right
 | `Top_And_Right
 | `Visual_Order_Left ] prop

val iso_comment : string prop
val jamo_short_name : string prop
val join_control : bool prop
val joining_group :
  [ `Ain | `Alaph | `Alef | `Alef_Maqsurah | `Beh | `Beth
  | `Burushaski_Yeh_Barree | `Dal | `Dalath_Rish | `E | `Farsi_Yeh | `Fe
  | `Feh | `Final_Semkath | `Gaf | `Gamal | `Hah | `Hamza_On_Heh_Goal
  | `He | `Heh | `Heh_Goal | `Heth | `Kaf | `Kaph | `Khaph
  | `Knotted_Heh | `Lam | `Lamadh
  | `Manichaean_Aleph | `Manichaean_Ayin | `Manichaean_Beth
  | `Manichaean_Daleth | `Manichaean_Dhamedh | `Manichaean_Five
  | `Manichaean_Gimel | `Manichaean_Heth | `Manichaean_Hundred
  | `Manichaean_Kaph | `Manichaean_Lamedh | `Manichaean_Mem
  | `Manichaean_Nun | `Manichaean_One | `Manichaean_Pe | `Manichaean_Qoph
  | `Manichaean_Resh | `Manichaean_Sadhe | `Manichaean_Samekh
  | `Manichaean_Taw | `Manichaean_Ten | `Manichaean_Teth
  | `Manichaean_Thamedh | `Manichaean_Twenty | `Manichaean_Waw
  | `Manichaean_Yodh | `Manichaean_Zayin
  | `Meem | `Mim | `No_Joining_Group
  | `Noon | `Nun | `Nya | `Pe | `Qaf | `Qaph | `Reh | `Reversed_Pe
  | `Rohingya_Yeh | `Sad | `Sadhe | `Seen | `Semkath | `Shin
  | `Straight_Waw | `Swash_Kaf
  | `Syriac_Waw | `Tah | `Taw | `Teh_Marbuta | `Teh_Marbuta_Goal | `Teth
  | `Waw | `Yeh | `Yeh_Barree | `Yeh_With_Tail | `Yudh | `Yudh_He | `Zain
  | `Zhain ] prop

val joining_type : [ `U | `C | `T | `D | `L | `R ] prop
val line_break :
  [ `AI | `AL | `B2 | `BA | `BB | `BK | `CB | `CJ | `CL | `CM | `CP | `CR | `EX
  | `GL | `H2 | `H3 | `HL | `HY | `ID | `IN | `IS | `JL | `JT | `JV | `LF | `NL
  | `NS | `NU | `OP | `PO | `PR | `QU | `RI | `SA | `SG | `SP | `SY | `WJ | `XX
  | `ZW ] prop

val logical_order_exception : bool prop
val lowercase : bool prop
val lowercase_mapping : [`Self | `Cps of cp list ] prop
val math : bool prop
val name : [`Pattern of string | `Name of string ] prop
(** In the [`Pattern] case occurences of the character ['#']
    ([U+0023]) in the string must be replaced by the value of the code
    point as four to six uppercase hexadecimal digits (the minimal
    needed). E.g. the pattern ["CJK UNIFIED IDEOGRAPH-#"] associated
    to code point [U+3400] gives the name ["CJK UNIFIED IDEOGRAPH-3400"].  *)

val name_alias :
  (string * [`Abbreviation | `Alternate | `Control | `Correction | `Figment])
list prop

val nfc_quick_check : [ `True | `False | `Maybe ] prop
val nfd_quick_check : [ `True | `False | `Maybe ] prop
val nfkc_quick_check : [ `True | `False | `Maybe ] prop
val nfkc_casefold : [`Self | `Cps of cp list] prop
val nfkd_quick_check : [ `True | `False | `Maybe ] prop
val noncharacter_code_point : bool prop
val numeric_type : [ `None | `De | `Di | `Nu ] prop
val numeric_value : [`NaN | `Frac of int * int | `Num of int64] prop
val other_alphabetic : bool prop
val other_default_ignorable_code_point : bool prop
val other_grapheme_extend : bool prop
val other_id_continue : bool prop
val other_id_start : bool prop
val other_lowercase : bool prop
val other_math : bool prop
val other_uppercase : bool prop
val pattern_syntax : bool prop
val pattern_white_space : bool prop
val quotation_mark : bool prop
val radical : bool prop

type script =
[ `Aghb | `Ahom | `Arab | `Armi | `Armn | `Avst
| `Bali | `Bamu | `Bass | `Batk | `Beng | `Bopo | `Brah | `Brai | `Bugi
| `Buhd
| `Cakm | `Cans | `Cari | `Cham | `Cher | `Copt | `Cprt | `Cyrl
| `Deva | `Dsrt | `Dupl
| `Elba | `Egyp | `Ethi
| `Geor | `Glag | `Goth | `Gran | `Grek | `Gujr | `Guru
| `Hang | `Hani | `Hano | `Hatr | `Hebr | `Hira | `Hluw | `Hmng | `Hrkt
| `Hung
| `Ital
| `Java
| `Kali | `Kana | `Khar | `Khmr | `Khoj | `Knda | `Kthi
| `Lana | `Laoo | `Latn | `Lepc | `Limb | `Lina | `Linb | `Lisu | `Lyci
| `Lydi | `Mahj
| `Mand | `Mani | `Mend | `Merc | `Mero | `Mlym | `Modi | `Mong | `Mroo
| `Mtei | `Mult | `Mymr
| `Narb | `Nbat | `Nkoo
| `Ogam | `Olck | `Orkh | `Orya | `Osma
| `Palm | `Pauc | `Perm | `Phag | `Phli | `Phlp | `Phnx | `Plrd | `Prti
| `Qaai
| `Rjng | `Runr
| `Samr | `Sarb | `Saur | `Sgnw | `Shaw | `Shrd | `Sidd | `Sind | `Sinh
| `Sora | `Sund | `Sylo | `Syrc
| `Tagb | `Takr | `Tale | `Talu | `Taml | `Tavt | `Telu | `Tfng | `Tglg
| `Thaa | `Thai | `Tibt | `Tirh
| `Ugar
| `Vaii
| `Wara
| `Xpeo | `Xsux
| `Yiii
| `Zinh | `Zyyy | `Zzzz ]

val script : script prop
val script_extensions : script list prop

val sentence_break :
  [ `AT | `CL | `CR | `EX | `FO | `LE | `LF | `LO | `NU | `SC | `SE | `SP
  | `ST | `UP | `XX ] prop

val simple_case_folding : [ `Self | `Cp of cp ] prop
val simple_lowercase_mapping : [ `Self | `Cp of cp ] prop
val simple_titlecase_mapping : [ `Self | `Cp of cp ] prop
val simple_uppercase_mapping : [ `Self | `Cp of cp ] prop
val soft_dotted : bool prop
val sterm : bool prop
val terminal_punctuation : bool prop
val titlecase_mapping : [`Self | `Cps of cp list ] prop
val uax_42_element : [ `Reserved | `Noncharacter | `Surrogate | `Char ] prop
(** Not normative, artefact of [Uucd]. Corresponds to the
    {{:http://www.unicode.org/reports/tr42/#w1aac13b9b1}XML element name}
    that describes the code point. *)

val unicode_1_name : string prop
val unified_ideograph : bool prop
val uppercase : bool prop
val uppercase_mapping : [`Self | `Cps of cp list ] prop
val variation_selector : bool prop
val white_space : bool prop
val word_break :
  [ `CR | `EX | `Extend | `FO | `KA | `LE | `LF | `MB | `ML | `MN | `NL | `NU
  | `RI | `XX | `DQ | `HL | `SQ ] prop

val xid_continue : bool prop
val xid_start : bool prop

(** {2:unihan Unihan properties}

    In alphabetic order. For now unihan properties are always
    represented as strings. *)

val kAccountingNumeric : string prop
val kAlternateHanYu : string prop
val kAlternateJEF : string prop
val kAlternateKangXi : string prop
val kAlternateMorohashi : string prop
val kBigFive : string prop
val kCCCII : string prop
val kCNS1986 : string prop
val kCNS1992 : string prop
val kCangjie : string prop
val kCantonese : string prop
val kCheungBauer : string prop
val kCheungBauerIndex : string prop
val kCihaiT : string prop
val kCompatibilityVariant : string prop
val kCowles : string prop
val kDaeJaweon : string prop
val kDefinition : string prop
val kEACC : string prop
val kFenn : string prop
val kFennIndex : string prop
val kFourCornerCode : string prop
val kFrequency : string prop
val kGB0 : string prop
val kGB1 : string prop
val kGB3 : string prop
val kGB5 : string prop
val kGB7 : string prop
val kGB8 : string prop
val kGradeLevel : string prop
val kGSR : string prop
val kHangul : string prop
val kHanYu : string prop
val kHanyuPinlu : string prop
val kHanyuPinyin : string prop
val kHDZRadBreak : string prop
val kHKGlyph : string prop
val kHKSCS : string prop
val kIBMJapan : string prop
val kIICore : string prop
val kIRGDaeJaweon : string prop
val kIRGDaiKanwaZiten : string prop
val kIRGHanyuDaZidian : string prop
val kIRGKangXi : string prop
val kIRG_GSource : string prop
val kIRG_HSource : string prop
val kIRG_JSource : string prop
val kIRG_KPSource : string prop
val kIRG_KSource : string prop
val kIRG_MSource : string prop
val kIRG_TSource : string prop
val kIRG_USource : string prop
val kIRG_VSource : string prop
val kJa : string prop
val kJHJ : string prop
val kJIS0213 : string prop
val kJapaneseKun : string prop
val kJapaneseOn : string prop
val kJis0 : string prop
val kJis1 : string prop
val kKPS0 : string prop
val kKPS1 : string prop
val kKSC0 : string prop
val kKSC1 : string prop
val kKangXi : string prop
val kKarlgren : string prop
val kKorean : string prop
val kLau : string prop
val kMainlandTelegraph : string prop
val kMandarin : string prop
val kMatthews : string prop
val kMeyerWempe : string prop
val kMorohashi : string prop
val kNelson : string prop
val kOtherNumeric : string prop
val kPhonetic : string prop
val kPrimaryNumeric : string prop
val kPseudoGB1 : string prop
val kRSAdobe_Japan1_6 : string prop
val kRSJapanese : string prop
val kRSKanWa : string prop
val kRSKangXi : string prop
val kRSKorean : string prop
val kRSMerged : string prop
val kRSUnicode : string prop
val kSBGY : string prop
val kSemanticVariant : string prop
val kSimplifiedVariant : string prop
val kSpecializedSemanticVariant : string prop
val kTaiwanTelegraph : string prop
val kTang : string prop
val kTotalStrokes : string prop
val kTraditionalVariant : string prop
val kVietnamese : string prop
val kXHC1983 : string prop
val kWubi : string prop
val kXerox : string prop
val kZVariant : string prop

(** {1:db Unicode character databases} *)

type block = (cp * cp) * string
(** The type for blocks. Code point range, name of the block. *)

type named_sequence = string * cp list
(** The type for named sequences. Sequence name, code point sequence. *)

type normalization_correction =
    cp * cp list * cp list * (int * int * int)
(** The type for normalization corrections.
    Code point, old normalization, new normalization, version *)

type standardized_variant =
    cp list * string * [ `Isolate | `Initial | `Medial | `Final ] list
(** The type for standarized variants. Code point sequence,
    description, when. *)

type cjk_radical = string * cp * cp
(** The type for CJK radicals. Radical number, CJK radical character,
    CJK unified ideograph. *)

type emoji_source = cp list * int option * int option * int option
(** The type for emoji sources. Unicode, docomo, kddi, softbank. *)

type t =
  { description : string;
    repertoire : props Cpmap.t;
    blocks : block list;
    named_sequences : named_sequence list;
    provisional_named_sequences : named_sequence list;
    normalization_corrections : normalization_correction list;
    standardized_variants : standardized_variant list;
    cjk_radicals : cjk_radical list;
    emoji_sources : emoji_source list;
}
(** The type for Unicode character databases.

    {b Note.} Absence of an optional top-level field in the database
    is denoted by the neutral element of its type (empty string, empty
    list, {!Cpmap.empty}).  This means that the module doesn't
    distinguish between absence of a field and presence of the field
    with empty data (but incurs no problems in this context). *)

val cp_prop : t -> cp -> 'a prop -> 'a option
(** [cp_prop ucd cp p] is the property [p] of the code point [cp]
    in [db]'s repertoire, if [p] is in the repertoire and the property
    exists for [p]. *)

(** {1:decoder Decode} *)

type src = [ `Channel of in_channel | `String of string ]
(** The type for input sources. *)

type decoder
(** The type for Unicode character database decoders. *)

val decoder : [< src] -> decoder
(** [decoder src] is a decoder that inputs from [src]. *)

val decode : decoder -> [`Ok of t | `Error of string ]
(** [decode d] decodes a database from [d] or returns an error. *)

val decoded_range : decoder -> (int * int) * (int * int)
(** [decoded_range d] is the range of characters spanning the [`Error]
    decoded by [d]. A pair of line and column numbers respectively one and
    zero based. *)

(** {1:basics Basics}

    The database and subsets of it for Unicode %%UNICODE_VERSION%% are
    available
    {{:http://www.unicode.org/Public/%%UNICODE_VERSION%%/ucdxml/}here}.
    Databases with groups should be preferred, they maximize value
    sharing and improve parsing performance.

    A database is decoded as follows:
{[
let ucd_or_die inf = try
  let ic = if inf = "-" then stdin else open_in inf in
  let d = Uucd.decoder (`Channel ic) in
  match Uucd.decode d with
  | `Ok db -> db
  | `Error e ->
    let (l0, c0), (l1, c1) = Uucd.decoded_range d in
    Printf.eprintf "%s:%d.%d-%d.%d: %s\n%!" inf l0 c0 l1 c1 e;
    exit 1
with Sys_error e -> Printf.eprintf "%s\n%!" e; exit 1

let ucd = ucd_or_die "/tmp/ucd.all.grouped.xml"
]}
    The convenience function {!cp_prop} can be used to query
    the property of a given code point. For example the
    {{!general_category}general category} of [U+1F42B]
    is given by:
{[
let u_1F42B_gc = Uucd.cp_prop ucd 0x1F42B Uucd.general_category
]}
*)

(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli.
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
