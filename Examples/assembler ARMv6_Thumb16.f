\ Meta assembler ARMv6_Thumb16 -- v0.7 (C) J.J. Hoekstra - 2022
\ all opcodes are checked against disassembler
\ added dreg bx,
\ added mov, movs, bkpt,
\ added almost all loads - not yet SP relative
\ added almost all stores - not yet SP relative
\ added in-place assembly and a system-specific 16 bit comma
\ added conditional branches with 8b imm
\ added branch with 11b imm - checked -> works fine
\ added add/sub with sp-specific opcodes

\ forget thumb16 : thumb16 ;        \ common way of deleting previous version before loading new version of code
unused

\ Add non standard .HEX & @+ & changed +TO regcnt
\ : .HEX ( u -- ) h. ;              \ For Win32Forth
\ : .HEX ( u -- ) base @ >r  hex  u.  r> base ! ; \ For Gforth

: BITCLEAR ( n m -- n_&&_invert_m ) \ for Win32Forth and Gforth
    invert and ;

: @+ ( a1 -- a2 u ) dup cell+ swap @ ;

vocabulary internal                 \ internal words
vocabulary thumb                    \ thumb assembler
also thumb
also internal definitions
also

decimal
0 value immbit
0 value shftbits
0 value offbits
0 value sp?
0 value regcnt
0 value bal?                        \ branch always flag - to do 11b imm iso 8b imm
0 value debug?                      \ change to state: debug || inline || meta-assembly

: BITFLD ( n opc lsl -- opc )       \ lsl of n, or into opc
     rot swap lshift or ;

: RESETOPC ( -- )                   \ resets flags for next opcode
    0 to immbit 0 to shftbits
    0 to offbits 0 to regcnt
    0 to sp? ;
resetopc

\ ** SYSTEM SPECIFIC ************** \
                                    \
: 16b, ( 16b -- )                   \ SYSTEM specific 16 bit comma for opcodes
    16bopcode,  ;                   \ compiles an 16b opcode and cleans/invalidates the 4 caches
                                    \
: 16b! ( 16b addr -- )              \ SYSTEM specific little-endian 16 bit store for opcodes
    over 255 and over safec!        \ 'safec!' in wabiForth stores 1 byte and cleans/invalidates the 4 caches
    swap 8 rshift 255 and swap      \ prepare high 8bits
    1 + safec! ;                    \
                                    \
\ ** SYSTEM SPECIFIC ************** \

: 16b@ ( addr -- 16b_value )
    c@+ swap c@ 8 lshift or ;

: EOFOPC ( opc -- )                 \ resets flags - handles debug/production
    debug? if
        ." opcode: " .hex cr
    else
        16b,
    then
    resetopc ;

: EOF32OPC ( opc opc -- )           \ resets flags - handles debug/production
    debug? if cr
        ." opcode_lo: " .hex 3 spaces
        ." opcode_hi: " .hex cr
    else
        16b,
        16b,
    then
    resetopc ;

thumb definitions
: I#, ( -- ) 1 to immbit ;

internal definitions
: REG ( n -- ) create , does> @ regcnt 1+ to regcnt ;
: RSP ( n -- ) create , does> @ regcnt 1+ to regcnt true to sp? ;

thumb definitions
 0 reg r0,       1 reg r1,       2 reg r2,       3 reg r3,       4 reg r4,
 5 reg r5,       5 reg top,      6 reg r6,       7 reg r7,       8 reg r8,
 9 reg r9,      10 reg r10,     11 reg r11,     12 reg r12,     13 rsp sp,
13 rsp r13,     14 reg lr,      15 reg pc,

\ -- Data processing opcodes -----

internal definitions
: INIOPCDP ( -- ) ;                 \ init of dataproc - empty at the moment
\ all data-processing checked against GNU assembler!!

: PUT2REGS ( r r opc -- opc )
    3 bitfld                        \ reg_n
    or ;                            \ reg_dm

: PUT2REGS_I ( r r i opc opc -- opc )
    immbit if                       \ if immbit set -> make opcode with immediate
        drop
        6 bitfld                    \ put imm5b in place
    else
        swap drop
    then
    put2regs ;

hex
: PUT1RI2R ( r r/i opc -- opc )
    regcnt 1 = if                   \ 1 reg -> imm8b
        2000 or
        or
        8 bitfld
    else
        put2regs
    then ;
decimal

: (2ROP ( n -- ) create , does> @ ( iniopcdp ) put2regs eofopc ;
: (2RIOP ( n m -- ) create  , , does> @+ swap @ put2regs_i eofopc ;
: (1RI_2R ( n -- ) create , does> @ ( iniopcdp ) put1ri2r eofopc ;

thumb definitions
\ the following opcodes have only 2 lower regs as option
hex  4000 (2rop ands,  4040 (2rop eors,
                       4140 (2rop adcs,  4180 (2rop sbcs,   41C0 (2rop rors,
     4200 (2rop tst,   4240 (2rop rsbs,                     42C0 (2rop cmn,
     4300 (2rop orrs,  4340 (2rop muls,  4380 (2rop bics,   43C0 (2rop mvns,
\ for rsbs,: leave out the immediate zero -> r1, r2, rsbs, = r1, r2, 0 i#, rsbs, as the zero is the only possible option

\ The following opcodes have both rr and rr plus imm as options each with different opcodes
     4100 1000 (2riop asrs,
     4080 0000 (2riop lsls,
     40C0 0800 (2riop lsrs,

\ The following opcode has 3 options -> #TBD
\     4140 (2rop cmp,               \ cmp, is ri8b or rr (both lower regs) or rdrd (not both lower regs)
\ check needed to see whether any of regs is hi -> different opcode

\ the following opcode have either rr or ri8b
      0000 (1ri_2r movs,  BE00 (1ri_2r bktp,

: cmp,
    regcnt 1 = if                   \ 1 reg -> imm8b
        2800
        or
        8 bitfld
    else
        4280
        put2regs
    then
    eofopc ;

internal definitions
decimal
\ -- special data processing -----
: PUT2DREGS ( drnd drm opc -- opc ) \ 2 hiregs
    3 bitfld                        \ reg_m - full 4 bits
    over >r                         \ copy reg_dn to r:stack
    swap 7 and                      \ isolate reg_dn_lower_3_bits
    or                              \ put lower 3bits at bitpositions 2:0
    r> 8 and 3 rshift               \ upper bit of reg_dn on stack
    swap 7 bitfld ;                 \ put upper bit reg_dn at position 7

hex
: PUTSPIMM ( sp imm7 opc/reg sp imm8 opc -- opc )
    cr ." regcnt: " regcnt . cr
    regcnt 2 = if
        drop                        \ drop old opcode
        2 arshift A800 or           \ new opcode and imm8
        nip                         \ drop sp, as it is part of the opcode
        8 bitfld                    \ 3b destination-reg_lo
    else
        drop 2 arshift B000 or      \ imm7
        nip                         \ drop sp, register as it is part of opcode
    then ;

: SPTRIAGE
    immbit 0<>
    sp? 0<>
    and if                          \ immediate && sp -> yes PUTSPIMM
        putspimm
    else
        put2dregs
    then ;

thumb definitions
: sub, ( sp, imm8 -- opcode, ) 2 arshift B080 or nip eofopc ; \ this is for sp, i#, imm8 sub, only!

internal definitions
: (SPDZ ( n -- ) create , does> @ put2dregs eofopc ;
: (SPIM ( n -- ) create , does> @ sptriage eofopc ;

thumb definitions
     4400 (spim add,  4600 (spdz mov,
decimal

\ A800 ADD <Rd>, SP, #<imm8> -> i#, mandatory and SP - imm8*4!! -> 0-1020
\   -> Rd=lo_reg, r0, sp, i#, imm8 add,
\ B000 ADD SP, SP, #<imm7> -> sp, i#, imm add, -> SP and 1reg imm*4!! -> 0-508
\   ->
\ 4468 ADD <Rdm>, SP, <Rdm> << this is the normal coding -> nothing to be done
\ 4485 ADD SP, <Rm> -> rm=hi op pos3 and higher
\   -> this is with a higher reg and normal coding
\ B080 SUB SP, SP, #<imm7> ->

\ -- 3 reg opcodes -----
internal definitions hex
create 3ropctbl
    3000 , 1C00 , 1800 ,            \ adds - ri rri rrr
    3800 , 1E00 , 1A00 ,            \ subs - ri rri rrr
decimal

: put3regs \ ( r r r indx -- opc ) or ( r r imm indx -- opc ) or ( r imm indx -- opc )
    regcnt + cells 3ropctbl + @     \ get opcode from table
    regcnt 1 = if                   \ 1 reg -> imm8b
        or
        8 bitfld
    else                            \ 2 regs -> 3b imm or 3 regs
        6 bitfld                    \ 3b_imm/reg_m
        3 bitfld                    \ reg_n
        or                          \ reg_d
    then ;

: (3ROP ( n -- ) create , does> @ put3regs eofopc ;
thumb definitions
    -1 (3rop adds,   2 (3rop subs,

internal definitions
\ -- 3 reg load store -----
hex create 3rlsctbl
    6800 , 5800 ,                   \ ldr   - rri5 rrr
                                    \ 4800 , -> ri8 is literal from pool *4
    8800 , 5A00 ,                   \ ldrh  - rri5 rrr
    7800 , 5C00 ,                   \ ldrb  - rri5 rrr
    0000 , 5E00 ,                   \ ldrsh - xxxx rrr
    0000 , 5600 ,                   \ ldrsb - xxxx rrr

    6000 , 5000 ,                   \ str   - rri5 rrr
                                    \ no store literal available in THUMB
    8000 , 5200 ,                   \ strh  - rri5 rrr
    7000 , 5400 ,                   \ strb  - rri5 rrr

hex
: divfac ( opcode -- div )
    >r
    r@ 6800 =
    r@ 6000 =
    or if
        2
    else
        r@ 8800 =
        r@ 8000 =
        or if
            1
        else
            0
        then
    then
    rdrop ;

hex
: put3regsls \ ( r r r indx -- opc ) or ( r r imm indx -- opc ) or ( r imm indx -- opc )
    regcnt 1 = if                   \ 1 reg -> imm8b
        drop 4800                   \ drop index and insert correct opcode
        swap 2 rshift swap          \ delen imm door 4
        or                          \ add imm at pos 0
        8 bitfld                    \ add rt
    else
        regcnt + cells 3rlsctbl + @ \ get opcode from table
                                    \ divide by 4, 2 or 1 for 6800, 8800, 7800
        dup divfac >r               \ r r i opc div
        swap r> rshift swap         \ 2 regs -> 3b imm or 3 regs
        6 bitfld                    \ 5b_imm/reg_m
        3 bitfld                    \ reg_n
        or                          \ reg_t
    then
    dup C rshift 0=
        abort" error in LDRSH or LDRSB - immediate index not allowed" ;
decimal

: (3RLS ( n -- ) create , does> @ put3regsls eofopc ;
thumb definitions
    -2 (3rls ldr,   0 (3rls ldrh,   2 (3rls ldrb,  4 (3rls ldrsh,   6 (3rls ldrsb,
     8 (3rls str,  10 (3rls strh,  12 (3rls strb,

internal definitions
\ -- zero reg opcodes -----
: (ZR create , does> @ eofopc ;
thumb definitions hex
    BF00 (zr nop,  BF10 (zr yield,  BF20 (zr wfe,
    BF30 (zr wfi,  BF40 (zr sev,
decimal


\ -- zero reg 32 bit opcodes -----
\ -- bariers -----

internal definitions
: (ZR32 create , , does> @+ swap @ eof32opc ;
thumb definitions hex
    8F5F F3BF (zr32 dmb,  8F4F F3BF (zr32 dsb,   8F6F F3BF (zr32 isb,
decimal

\ -- 1 full reg -----
\ -- branch and exchange -----

internal definitions
: put1dreg 3 bitfld ;               \ this will be extended thus a seperate word

: (1DR create , does> @ put1dreg eofopc ;
thumb definitions hex
    4700 (1DR bx,   4780 (1dr blx,
decimal

\ -- branches and labels -----
internal definitions
0 value ORIGOPC                     \ contained in the offsetbits of the not-yet-resolved branch
create LBLTBL 16 2* cells allot     \ =label-table - table with label-addresses

: LBL[ lbltbl 32 cells 0 fill ; immediate \ clears the labeltable

: lbl#2addr ( lbl# 8_11_fl -- addr ) - cells lbltbl + ;

: LBL@ ( lbl# 8_11_fl -- n=addr ) lbl#2addr @ ;
: LBL! ( n=addr lbl# 8_11_fl -- ) lbl#2addr ! ;


hex
: BOFFSET ( dest src 8_11_fl -- 8/11b_offset ) \ 4 byte pipeline in thumb
    if
        4 + - 1 arshift 7FF and     \ 11b offset -2048 to 2046 bytes
    else
        4 + - 1 arshift FF and      \ 8b offset -256 to 254 bytes
    then ;

0 value 8_11?

: CALCADDR ( old_address opcode+offset 8_11_fl - next_address/0x0 )
    if
        7FF and                     \ get offset to previous address with unresolved jump
        15 lshift 14 arshift
    else
        FF and                      \ get offset to previous address with unresolved jump
        18 lshift 17 arshift        \ HEX! - 8bits extend_sign and times 2 #bal
    then

    dup 0<> if
        +                           \ ( addr+offset*2 )
    else
        nip                         \ get rid of old_address under the 0x0
    then ;

: RESOLVE) ( addr 8_11_fl -- )      \ resolution of jumps in a linked list - called by LABELn:
    to 8_11?                        \ resolves for 8b and 11b
    begin
        >r                          \ keep address on r: for later use
        r@ 16b@                     \ get opcode
        to origopc                  \ keep fetched opcode for later use by calcadrr

        here r@ 8_11? boffset       \ get offset-bits - jump forward -> dest source

        origopc                     \ contains opcode-code
        8_11? if
            F800 and                \ keep the opcode-part
        else
            FF00 and                \ ditto
        then

        or r@ 16b!                  \ or opcode and offset and ! -> ready opcode

        r> origopc 8_11? calcaddr   \ ( new_addr ) get next addres from original value
        dup 0=
    until drop ;                    \ if 0x0 -> no further links and drop 0x0, otherwise next link

: RESOLVE ( lbl# -- )               \ checks if resolve of forward branch is needed - on yes resolves
    dup
    false lbl@                      \ get addr from lbltbl for 8b_imm
    dup 1 and if                    \ if bit<0> set -> link_address available
        1 bitclear false resolve)   \ -> resolve opcodes before the label
    else
        drop
    then

    true lbl@                       \ get addr from lbltbl+1 for 11b_imm
    dup 1 and if                    \ if bit<0> set -> link_address available
        1 bitclear true resolve)    \ -> resolve opcodes before the label
    else
        drop
    then ;

: HERE2LBL ( lbl# -- )
    here swap                       \ ( here lbl# )
    over over                       \ ( here lbl# here lbl# )
    false lbl!
     true lbl! ;

: LB# create , does> @ dup resolve here2lbl ;
thumb definitions
    00 lb# label0:  02 lb# label1:  04 lb# label2:  06 lb# label3:
    08 lb# label4:  0A lb# label5:  0C lb# label6:  0E lb# label7:

internal definitions
: LINK2LBLTBL ( lbl# -- ) here 1 or swap bal? lbl! ;

: NXTLBLLNK ( lbl# addr -- offset_bits )
\ link_address of latest unresolved branch to lbltbl
\ and link in offset bits to previous unresolved branch for later OR with opcode
    here - 1 arshift                \ ( lbl# offset_bits ) offset to earlier unresolved branch
    bal? if
        7FF and                     \ 11b imm
    else
        FF and                      \ 8b imm
    then
    swap                            \ ( offset_bits lbl# )
    link2lbltbl ;                   \ ( offset_bits ) put address of latest unresolved opcode in labeltable ( offset )

: LBLADDR ( lbl# -- br_offset/offset_to_linkaddress/0x0 )
    dup bal? lbl@ dup               \ ( lbl# addr_from_tbl addr_from_tbl )
    0= if                           \ >> label not yet defined -> 0x0 as signal for last link in list
        drop link2lbltbl
        0                           \ the zero is orred in the next with the avaiable opcode for the branch -> no effect
    else                            \ ( lbl# addr )
        dup 1 and if                \ check bit<0> - if set -> address of unresolved opcode ( lbl# addr )
            1 bitclear              \ now correct address
            nxtlbllnk               \ handle creation of link to earlier unresolved branch
        else nip                    \ -> branch backward - drop lbl#
            here bal? boffset       \ the offset bits are orred with the opcode in the step after this word
        then
    then ;


: (LB# create , does> @ lbladdr or eofopc ;
thumb definitions
    00 (lb# >lbl0,  02 (lb# >lbl1,  04 (lb# >lbl2,  06 (lb# >lbl3,
    08 (lb# >lbl4,  0A (lb# >lbl5,  0C (lb# >lbl6,  0E (lb# >lbl7,

internal definitions
: (BRH create , does> @ ( or ) false to bal? ( eofopc ) ;
: (BAL create , does> @ ( or )  true to bal? ( eofopc ) ;

thumb definitions
hex D000 (brh beq,  D100 (brh bne,  D200 (brh bcs,  D300 (brh bcc,
    D400 (brh bmi,  D500 (brh bpl,  D600 (brh bvs,  D700 (brh bvc,
    D800 (brh bhi,  D900 (brh bls,  DA00 (brh bge,  DB00 (brh blt,
    DC00 (brh bgt,  DD00 (brh ble,  E000 (bal b,
    D200 (brh bhs,  D300 (brh blo,  E000 (bal bal, \ << synonyms of bcs, bcc, b,
decimal

forth definitions previous previous

unused - cr . .( bytes - assembler total size)
