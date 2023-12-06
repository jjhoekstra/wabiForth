\ Meta assembler ARMv6_Thumb16 -- v0.14 (C) J.J. Hoekstra - 2022
\ 44924 bytes incl. 16k META table

\ ANSI: the following features are required:
\       32b cells
\       case-insensitivity
\       32b words can be stored and fetched 2 byte aligned

\ added dreg bx,
\ added mov, movs, bkpt,
\ added almost all loads - not yet SP relative
\ added almost all stores - not yet SP relative
\ added in-place assembly and a system-specific 16 bit comma
\ added conditional branches with 8b imm
\ added branch with 11b imm - checked -> works fine
\ added add/sub with sp-specific opcodes
\ added first version meta-assembler
\ rewrite from scratch of label-system - now suitable for BL and literal pool
\ added literal pool basics -> 4 literal_labels - LDR r1, label becomes: r1, LIT, label
\ unresolved opcodes now in a circular buffer for better performance and use of memory
\ all labels can be re-defined without clearing the label-system
\ only 1 kind of label - labels can be user-defined
\ removed ARSHIFT and +TO from source
\ added experimental UF2 functionality (presently max 256 bytes of executable)
\ added PC and SP relative LDR, and STR,
\ added SXTH, SXTB, UXTB, UXTH, REV, REV16, REVSH, SVC, BKPT,
\ added MSR, MRS, and special_register names
\ added LDM, LDMIA, LDMFD, STM, STMIA, STMEA, POP and PUSH
\ added BL, and ADR, --->> FINISHED WITH ALL OPCODES!! <<---
\ added L, -> commas the address of a label - makes ITC easy in the assembler

\ -- tasks tentativeley planned -----
\ #TBD: move from wabiForth specific safe! etc to normal
\       stores/fetch/comma with wabiForth specifics in optional words
\ #TBD: add 'internals' vocabulary back
\ #TBD: correct the distance check of branches - incorrect for jumps back
\ #TBD: get rid of '},' by combining with the opcodes LDM, and STM, et.al.
\ #TBD: add more checks, start with check on high and low regs


\ forget thumb16 : thumb16 ;        \ on wabiForth deletes previous version before loading new version
\ only forth definitions            \ ditto
unused

\ -- printing and service routines -----
: .HEX      ( u -- )                \ Cell wide version of .HEX uses four or more digits
   base @ >r  hex  0 <# # # # #s #> type space  r> base ! ;
: .BYTE      ( u -- )               \ print byte in HEX with 2 digits
   base @ >r  hex  0 <# # # #> type  r> base ! ;

: BITCLEAR ( n m -- n_&&_invert_m ) \ for Win32Forth and Gforth
    invert and ;

: @+ ( a1 -- a2 u ) dup cell+ swap @ ;

vocabulary internal                 \ internal words
vocabulary thumb                    \ thumb assembler
also thumb
also internal
also

thumb definitions
decimal
0 value nextlabel#                  \ to give consecutive numbers to labels
0 value #intllbls                   \ to count internal labels - for reset/clear purposes
0 value immbit
0 value sp?
0 value rhi?
0 value lr?
0 value pc?
0 value regcnt
0 value offsmethod                  \ method used for offset bits - 8b opc, 8b data, b11b opc, 24b opc, 8b adr
0 value postopc?                    \ used by label system to see if a label is pre or post of opcode
0 value action?                     \ 1=compile, 2=meta, 3 and higher still free
0 value debug?                      \ true->verbose - false->no text

16 1024 * constant SZEMETBUF        \ #QQ: what is a decent size for the metabuffer?
create METABUFFER szemetbuf allot
0 value MHERE metabuffer to mhere

: PPT  true to postopc? ;
: PPF false to postopc? ;

: CLRMETBUF ( addr n ch -- ) metabuffer szemetbuf 0 fill ;

\ thumb definitions
: META: ( -- )                      \ use meta: to initialise meta-assembler
    clrmetbuf
    metabuffer to mhere
    2 to action? ;

: ASM: 1 to action? ;               \ use asm: to initialise the assembler
asm:

: VERBOSE true to debug? ;          \ verbose mode
: NONVERBOSE false to debug? ;      \ nonverbose mode
nonverbose

\ internal definitions
: BITFLD ( n opc lsl -- opc )       \ lsl of n, or into opc
     rot swap lshift or ;

: RESETOPC ( -- )                   \ resets flags for next opcode
    0 to immbit   0 to regcnt
    0 to sp?      0 to rhi?
    0 to lr?      0 to pc?
    ppf ;
resetopc

: 1+MHERE mhere 1+ to mhere ;

: 16BMETA, ( 16b -- )               \ uses and raises mhere
    debug? if
        cr dup mhere ." meta-addr: " .hex .hex
    then
    dup      255 and mhere c! 1+mhere
    8 rshift 255 and mhere c! 1+mhere ;

\ ** SYSTEM SPECIFIC ************** \
                                    \
: 16B, ( 16b -- )                   \ SYSTEM specific 16 bit comma for opcodes
    debug? if cr                    \
        here ." addr: " .hex        \
        dup .hex                    \
    then                            \
    16bopcode,  ;                   \ compiles an 16b opcode and cleans/invalidates the 4 caches
                                    \
: 16B! ( 16b addr -- )              \ SYSTEM specific little-endian 16 bit store for opcodes
    debug? if cr                    \
        here ." addr rw: " .hex     \
        dup .hex                    \
    then                            \
    over 255 and over safec!        \ 'safec!' in wabiForth stores 1 byte and cleans/invalidates the 4 caches       \
    swap 8 rshift 255 and swap      \ prepare high 8bits
    1 + safec! ;                    \
                                    \
: SAFE! ( 32b addr -- )             \ stores a 32b value cache-safe - machine specific
    debug? if cr                    \
        here ." addr: " .hex        \
        dup .hex                    \
    then                            \
    safe! ;                         \ for other systems a normal STORE can likely be used
                                    \
\ ** SYSTEM SPECIFIC ************** \

: 16B@ ( addr -- 16b_value )
    c@+ swap c@ 8 lshift or ;

: EOFOPC ( opc -- )                 \ writes opcode @ HERE or METAHERE
    action? 1 = if
        16b,
    else
        16bmeta,
    then
    resetopc ;                      \ resets flags

: EOF32OPC ( opc opc -- )           \ 32b version of eofopc
    action? 1 = if
        16b, 16b,
    else
        16bmeta, 16bmeta,
    then
    resetopc ;

\ thumb definitions
: I#, ( -- ) 1 to immbit ;

: .METARAW ( -- )
    cr ." addr  opc"
    cr ." ====  ===="
    mhere metabuffer do
        cr i dup .hex 16b@ .hex
    2 +loop ;

: .META ( -- )
    cr ." addr  opc"
    cr ." ====  ===="
    mhere metabuffer do
        cr i metabuffer - .hex i 16b@ .hex
    2 +loop ;

: UF2_HEADER                        \ as wabiForth has no file system, this is a demo only
    cr ." 0A324655"                 \ 1st magic number
       ." 9E5D5157"                 \ 2nd magic number
    cr ." 00002000"                 \ flags -> fam-ID present
       ." 20000000"                 \ this is start of RAM
    cr ." 00000100"                 \ size datablock = 256 bytes
       ." 00000000"                 \ sequential block number - starts at 0x0
    cr ." 00000001"                 \ total number of blocks - keep the same throughout transfer
       ." E48BFF56" ;               \ family-ID Pico

: UF2_FOOTER
    ." 0AB16F30"    ;               \ 3rd and closing magic number

0 value DATABLOCKCNT
: UF2_DATA
    0 to datablockcnt cr
    mhere metabuffer do
        i c@ .byte
        datablockcnt 1+ to datablockcnt
        datablockcnt 7 and 0= if cr then
    loop
    476 datablockcnt - 0 do
        0 .byte
        datablockcnt 1+ to datablockcnt
        datablockcnt 7 and 0= if cr then
    loop ;

: .UF2 uf2_header uf2_data uf2_footer ;

\ -- registers -----

\ internal definitions
: INCRREGCNT regcnt 1+ to regcnt ;
: RHI_T true to rhi? ;

: REG ( n -- ) create , does> @ incrregcnt ;
: RSP ( n -- ) create , does> @ incrregcnt true to sp? rhi_t ;
: RPC ( n -- ) create , does> @ incrregcnt true to pc? rhi_t ;
: RLR ( n -- ) create , does> @ incrregcnt true to lr? rhi_t ;
: RHI ( n -- ) create , does> @ incrregcnt rhi_t ;

\ thumb definitions
 0 reg r0,       0 reg w,
 1 reg r1,       1 reg x,
 2 reg r2,       2 reg y,
 3 reg r3,       3 reg z,
 4 reg r4,       4 reg fls,
 5 reg r5,       5 reg top,
 6 reg r6,       6 reg ip,
 7 reg r7,       7 reg dst,
 8 rhi r8,       9 rhi r9,      10 rhi r10,     11 rhi r11,     12 rhi r12,
13 rsp sp,      14 rlr lr,      15 rpc pc,

\ -- Data processing opcodes -----

\ internal definitions
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

: (2ROP ( n -- ) create , does> @ put2regs eofopc ;
: (2RIOP ( n m -- ) create  , , does> @+ swap @ put2regs_i eofopc ;
: (1RI_2R ( n -- ) create , does> @ put1ri2r eofopc ;

\ thumb definitions
\ the following opcodes have only 2 lower regs as option
hex  4000 (2rop ands,  4040 (2rop eors,
                       4140 (2rop adcs,  4180 (2rop sbcs,   41C0 (2rop rors,
     4200 (2rop tst,   4240 (2rop rsbs,                     42C0 (2rop cmn,
     4300 (2rop orrs,  4340 (2rop muls,  4380 (2rop bics,   43C0 (2rop mvns,

     B200 (2rop sxth,  B240 (2rop sxtb,  B2C0 (2rop uxtb,   B280 (2rop uxth,
     BA00 (2rop rev,   BA40 (2rop rev16, BAC0 (2rop revsh,

\ for rsbs,: leave out the immediate zero -> r1, r2, rsbs, = r1, r2, 0 i#, rsbs, as the zero is the only possible option

\ The following opcodes have both rr and rr plus imm as options each with different opcodes
     4100 1000 (2riop asrs,
     4080 0000 (2riop lsls,
     40C0 0800 (2riop lsrs,

\ the following opcode have either rr or ri8b
      0000 (1ri_2r movs,  BE00 (1ri_2r bkpt,

\ : cmp,
\    regcnt 1 = if                   \ 1 reg -> imm8b
\        2800
\        or
\        8 bitfld
\    else
\        4280
\        put2regs
\    then
\    eofopc ;

\ internal definitions
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
: PUTSPIMM ( 'sp imm7 opc'/'reg sp imm8 opc' -- opc )
    regcnt 2 = if                   \ reg, add sp to reg and put in sp
        drop                        \ drop old opcode
        2/ 2/ A800 or
        nip                         \ drop sp, as it is part of the opcode
        8 bitfld                    \ 3b destination-reg_lo
    else
        drop 2/ 2/ B000 or
        nip                         \ drop sp, register as it is part of opcode
    then ;

: SPTRIAGE
    immbit 0<>
    sp? 0<>                         \ set if PC, is used at all
    and if                          \ immediate && sp -> yes PUTSPIMM
        putspimm
    else                            \ difference between pc, <reg> & <reg>, pc, to be done!!
        put2dregs
    then ;

: CMPTRIAGE ( r r opc / r i opc / rhi rhi opc -- r r opc )  \ triage between the 3
    rhi? if                         \ if any of the regs is hi -> change opcode
        drop 4500
        put2dregs
    else
        regcnt 1 = if               \ 1 reg -> imm8b
            drop 2800
            or
            8 bitfld
        else
            put2regs
        then
    then ;

\ : cmp,
\    regcnt 1 = if                   \ 1 reg -> imm8b
\        2800
\        or
\        8 bitfld
\    else
\        4280
\        put2regs
\    then
\    eofopc ;

\ thumb definitions
: SUB, ( sp, imm8 -- opcode, )
    2/ 2/ B080 or nip eofopc ;      \ this is for sp, i#, imm8 sub, only!

\ internal definitions
: (SPDZ ( n -- ) create , does> @ put2dregs eofopc ;
: (SPIM ( n -- ) create , does> @ sptriage eofopc ;

: (2RCMP ( n -- ) create , does> @ cmptriage eofopc ; \ #TBD for CMP only

\ The following opcode has 3 options: ri, rr or hirhir
     4280 (2rcmp cmp,               \ cmp, is ri8b or rr (both lower regs) or rdrd (not both lower regs)

\ thumb definitions
     4400 (spim ADD,  4600 (spdz MOV,
decimal

\ A800 ADD <Rd>, SP, #<imm8> -> i#, mandatory and SP - imm8*4!! -> 0-1020
\   -> Rd=lo_reg, r0, sp, i#, imm8 add,
\ B000 ADD SP, SP, #<imm7> -> sp, i#, imm add, -> SP and 1 reg imm7*4!! -> 0-508
\   ->
\ 4468 ADD <Rdm>, SP, << this is normal coding -> default 2 hi regs
\ 4485 ADD SP, <Rdm>, << this is normal coding -> default 2 hi regs
\ B080 SUB SP, SP, #<imm7> ->

\ -- 3 reg opcodes -----
\ internal definitions
create 3ropctbl
hex 3000 , 1C00 , 1800 ,            \ adds - ri rri rrr
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
\ thumb definitions
    -1 (3rop adds,   2 (3rop subs,

\ internal definitions
\ -- 3 reg load store -----
hex create 3rlsctbl
    6800 , 5800 ,                   \ ldr   - rri5 rrr
    8800 , 5A00 ,                   \ ldrh  - rri5 rrr
    7800 , 5C00 ,                   \ ldrb  - rri5 rrr
    0000 , 5E00 ,                   \ ldrsh - xxxx rrr
    0000 , 5600 ,                   \ ldrsb - xxxx rrr
    6000 , 5000 ,                   \ str   - rri5 rrr
    8000 , 5200 ,                   \ strh  - rri5 rrr
    7000 , 5400 ,                   \ strb  - rri5 rrr

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

0 value indx=
0 value reg=
: PC/SPOPC ( -- opc/0x0 )
    regcnt 2 = if
        indx= -2 = if
            reg= D = if
                9800
            else
                reg= F = if
                    4800
                else 0 then
            then
        else
            indx= 8 =
            reg= D = and if
                9000
            else 0 then
        then
    else
        0
    then ;

: put3regsls \ ( r r r indx -- opc ) or ( r r imm indx -- opc ) or ( r imm indx -- opc )
\ first handle the special cases of PC and SP relative
    to indx=  over to reg=          \ indx in indx= - copy of 3rd in reg=
                                    \ D=13=PCreg >> r0, sp, 255 ldr, = SP relative
                                    \ F=15=PCreg >> r0, pc, 255 ldr, = PC relative
    pc/spopc                        \ get correct opcode or 0x0
    dup if                          \ ( r r imm opc )
        swap 2 rshift or            \ divide imm by 4 and add at pos 0
        swap drop                   \ drop SP or PC
        8 bitfld                    \ add rt
        exit                        \ ready
    then

\ than handle all other LDR/STR cases
    drop indx=                      \ get rid of 0x0 put indx back on stack
    regcnt + cells 3rlsctbl + @     \ get opcode from table using indx
    dup divfac >r                   \ divide by 4, 2 or 1 - ( r r i opc div )
    swap r> rshift swap             \ 2 regs -> 3b imm or 3 regs
    6 bitfld                        \ 5b_imm/reg_m
    3 bitfld                        \ reg_n
    or                              \ reg_t

\ check if immediate index was used on LDRSH or LDRSB
    dup C rshift 0=                 \ << if opcode equals 0x0
        abort" error in LDRSH or LDRSB - immediate index not supported by Thumb"
; decimal

\ there are 4 LDR versions
\ LDR rt, [ rn + 5b*4 ] - 2 regs
\ LDR rt, [ SP + 8b*4 ] - 2 regs & SP is inherent in opcode <- flow-control
\ LDR rt, [ PC + 8b*4 ] - 2 regs & PC is inherent in opcode <- literal pool -> LIT,
\ LDR rt, [ rn + rm ]   = 3 regs

: (3RLS ( n -- ) create , does> @ put3regsls eofopc ;
\ thumb definitions
    -2 (3rls ldr,   0 (3rls ldrh,   2 (3rls ldrb,  4 (3rls ldrsh,   6 (3rls ldrsb,
     8 (3rls str,  10 (3rls strh,  12 (3rls strb,


\ -- block transfer -----
hex 9009 constant REGMARKER
0 value REGBITS
: putregsbt ( reg opc -- opc )
    dup BC00 = if               \ POP?
        pc? if
            100 or              \ set P-bit if PC-reg is used
        then
        regbits or exit         \ set the register bits
    then
    dup B400 = if               \ PUSH?
        lr? if
            100 or              \ set M-bit if LR is used
        then
        regbits or exit         \ set the register bits
    then
    regbits or                  \ regs to be saved
    8 bitfld ;                  \ base-reg address where regs are saved/loadedfrom

: SETBIT ( n -- ) \ sets bit n in value regbit
    dup F = if                  \ F=15=PC
        drop true to pc? exit   \ drop pc=reg from list an dset flag
    then
    dup E = if                  \ E=14=lr
        drop true to lr? exit   \ ditto for lr
    then
    dup 7 > if cr ." low register expected -> " abort then
    1 swap lshift regbits or to regbits ;

: BLREGS ( reg*x -- )           \ gets reg numbers/sets bits till regmarker is seen
    9 0 do                      \ max 9 regs 0-7 & PC/LR
        dup regmarker = if leave then
        setbit
    loop drop ;                 \ drop regmarker

: r-r, ( reg1 reg2 -- )         \ reads 2 regs and setsbits in range of reg1 to reg2
    2dup > if swap then         \ wrong order -> correct order - could also be abort
    2dup = if drop setbit       \ 2 equal regs -> set 1 bit only
    else
        1+ swap do i setbit loop
    then ;

: {, regmarker 0 to regbits ;   \ start of reg list -> marker on stack >> 0x9009
: }, blregs ;                   \ #TBD: reduce to 1 word & opcode closes the list

: BLTR ( n -- ) create , does> @ putregsbt eofopc ;
    C800 bltr ldm,   C800 bltr ldmia,   C800 bltr ldmfd,    \ all 3 the same!
    C000 bltr stm,   C000 bltr stmia,   C000 bltr stmea,    \ ditto
    BC00 bltr pop,   B400 bltr push,

\ There is no difference in LDM Rn! and LDM Rn in the opcode
\ The difference is only within the CPU: If Rn is not part of the list, write-back occurs
\ for STM ditto


\ internal definitions
\ -- zero reg opcodes -----
: (ZR create , does> @ eofopc ;
\ thumb definitions
    BF00 (zr nop,    BF10 (zr yield,    BF20 (zr wfe,
    BF30 (zr wfi,    BF40 (zr sev,

    B672 (zr cpsid,  B662 (zr cpsie,


\ == imm8b zero reg -----
: (ZRI8 create , does> @ or eofopc ;    \ always use immediate -> fi.: 0 svc,
    BF00 (zri8 svc,  BE00 (zri8 bkpt,   DE00 (zri8 udf,


\ -- zero reg 32 bit opcodes -----
\ -- bariers -----

\ internal definitions
: 16bsplit ( 32b -- 16b 16b )
    dup FFFF and swap 10 rshift ;   \ 10=16

: (ZR32 create , does> @ 16bsplit eof32opc ;
\ thumb definitions
     F3BF8F5F (zr32 dmb,   F3BF8F4F (zr32 dsb,    F3BF8F6F (zr32 isb,


\ -- 32bit with special register -----
\ MRS MSR
: MRS, ( rd spec_reg -- opc )       \ all regs
    F3EF8000 or 8 bitfld 16bsplit eof32opc ;
: MSR, ( spec_reg rd -- opc )
    F3808800 10 bitfld or 16bsplit eof32opc ; \ 10=16

: (sr create , does> @ ;
    00 (sr apsr, 01 (sr iapsr,   02 (sr eapsr, 03 (sr xpsr,
    05 (sr ipsr, 06 (sr epsr,    07 (sr iepsr, 08 (sr msp,
    09 (sr psp,  10 (sr primask, 14 (sr control,

\ -- 1 hi reg -----
\ -- branch and exchange -----

\ internal definitions
: put1dreg 3 bitfld ;               \ this probably will be extended thus a seperate word

: (1DR create , does> @ put1dreg eofopc ;
\ thumb definitions
    4700 (1dr bx,   4780 (1dr blx,
decimal

\ **************  BRANCHES and LABELS  ****************
\ new table based version:
\ table with label-addressen -> 1 column with 32b addressen - 0x0=: no label-address known
\ table with unsolved opcodes -> columns: label#, kind of calc needed, addres of empty label
\ pointer into table of next free row

\ internal definitions

decimal
0 value *OPCADR                     \ index into opcadr-table - points to next available row
256 constant #LABELS
 64 constant #OPCADR                \ max=254! as FF is 'empty'
create LBLADR #labels cells allot   \ =label-table - table with label-addresses
create OPCADR #opcadr 6 * allot     \ per ROW: 1 byte=label#, 1 byte=method, 4 bytes=address

decimal
: LBL#ADR ( lbl# -- addr ) cells lbladr + ;
: LBLADR@ ( lbl# -- addr ) lbl#adr @ ;
: LBLADR! ( addr lbl# -- ) lbl#adr ! ;

: NEWLABEL#
    nextlabel#
    dup 1+ to nextlabel#
    nextlabel# #labels >= abort" too many new labels defined " ;

: ADROPCADR ( index -- addr ) 6 * opcadr + ;

: LBL[
    lbladr #labels cells 0 fill     \ clears table with label addresses
    opcadr #opcadr 6 * 0 fill       \ and table with opcode addresses
    #opcadr 0 do
        -1 i adropcadr c! loop      \ put no-label_flag=-1 in label-column
    0 to *opcadr                    \ index-pointer into opcadr-table
    #intllbls to nextlabel#         \ keep internal label-numbers intact
; immediate

: OPCADRLBL@ ( index -- lbl# )      \ gets label value from row(index) from opcadr-table
    adropcadr c@ ;
: OPCADRLBL! ( 8b_value index -- )  \ puts a 8bit value in label_field in row(index)
    adropcadr c! ;
: OPCADRMTDADR@ ( index -- addr_open_opcode method )
    adropcadr >r
    r@ 2 +  @                       \ addr as a 32b word
    r>  1+ c@ ;                     \ method as a byte

: NEXT*OPCADR ( -- )
    *opcadr 1+ #opcadr  = if        \ reached end of circ buffer?
        0 to *opcadr
    else
        *opcadr 1+ to *opcadr
    then ;

: FREE*OPCADR ( -- )                \ finds next free row in opcadr-table or ABORT
    #opcadr 0 do                    \ a maximum of #opcadr tries is enough to find a free spot or not
        *opcadr adropcadr c@
        255 = if                    \ if empty -> next free row in table opcard found
            unloop exit
        else
            next*opcadr
        then
    loop
    cr ." too many concurrent unresolved labels " abort ;

: .LBL                              \ for debug
\    #labels 0 do
    nextlabel# 0 do
        cr i .byte space i lbladr@ .hex
    loop ;

: .OPC                              \ for debug
    #opcadr 0 do
        cr
        i opcadrlbl@ ." lbl: " .byte space
        i opcadrmtdadr@ ." mtd: " .byte space ." addr: " .hex
    loop ;

\ : M/HERE ( -- mhere/here )        \ 16c && 18 opcodes - << 'bad' Forth
\   action? 1 = if                  \ 64c && 10 opc subroutine threaded
\        here                       \ normal compilation
\    else                           \ meta compilation
\        mhere
\    then ;

: M/HERE                            \ 33c && 28 opcodes - << 'good' Forth - why?
    action? 1 = dup here and        \ 103c && 13 opc subroutine threaded
    swap invert mhere and or ;

hex
: 4BALIGN ( dest -- 4baligned(dest) )
    dup 2 and if 2 - then ;

: VALDIS? ( actual_distance max_distance -- ) \ aborts on distance too long
    2dup > ABORT" distance between source and dest above limit" ; \ #BUG for neg numbers!!

: DISTANCE ( dest src -- delta ) 4 + - 2/ ;

0 value SBIT
: MAKEJ ( dist bit# -- J_bit )
    rshift 1 and                    \ isolate distance:bit_n
    invert sbit xor 1 and ;         \ do the calc

: MAKEBLBITS ( distance -- bits )    \ f***ing difficult for a simple +/-24b branch-link...
\  S = signbit - at bit26 van 32b opcode
\ I1 = NOT( S eor J1 ) - J1=bit13 -> J1 = NOT ( S eor distance:bit22 )
\ I2 = NOT( S eor J2 ) - J2=bit11 -> J2 = NOT ( S eor distance:bit21 )
\ imm10 at bit16 and higher = distance:bit20-11
\ imm11 at bit00 and higher = distance:bit10-00

    >r

\   r@ 1F rshift 1 and if           \ clearly structered 'bad' forth - runs in ~17 cycles
\       4000000                     \ start number depends on sign of distance
\       1 to sbit                   \ and set signbit for next step
\   else
\       0 0 to sbit                 \ start with 0 and clear signbit
\   then

    r@ 1F rshift 1 and 1= dup       \ 'good' cryptic forth - runs in ~25 cycles
    4000000 and swap 1 and to sbit

    r@ 16 makej D lshift or         \ 16=22 - make J1 - D=13 - put at bit13
    r@ 15 makej B lshift or         \ 15=22 - make J2 - B=11 - put at bit11

    r@ B rshift 3FF and             \ B=11 - isolate distance:bit20-11
    10 lshift or                    \ 10=16 - put at bit25-16

    r> 7FF and or ;                 \ isolate distance:bit10-0 and put at bit10-0

: HERE2LBL ( lbl# -- ) m/here swap LBLADR! ;

\ in thumb PC is 4 bytes ahead of opcode being executed
: 8BCONOFF ( dest src -- offset )   \ =offsmethod 0
    distance FF valdis? and ;       \ 8b conditinal branch to opcode offset -256 to 254 bytes
: 8BLITOFF ( ditto )                \ =offsmethod 1
    4balign
    distance 2/ FF valdis? and ;    \ 8b literal data offset 0 to 1020 bytes (4 byte aligned)
: 8BADROFF ( ditto )                \ =offsmethid 4
    distance 2/ FF valdis? and ;    \ 8b literal data offset 0 to 1020 bytes (4 byte aligned)
: 11BOPCOFF ( ditto )               \ =offsmethod 2
    distance 7FF valdis? and ;      \ 11b branch to opcode offset -2048 to 2046 bytes
: 24BBLXOFF ( ditto )               \ =offsmethod 3
    distance FFFFFF valdis?         \ 24b BL to opcode offset -16777216 to 16777214 bytes
    makeblbits and ;
: 32BADROFF ( ditto )               \ strictly speaking the is 32b offset with opcode 0x0
    drop ;                          \ drop the src - thats it!

: CALCOFFSET ( dest src -- offset_bits )
    offsmethod 0 = if  8bconoff exit then
    offsmethod 1 = if  8blitoff exit then
    offsmethod 2 = if 11bopcoff exit then
    offsmethod 3 = if 24bblxoff exit then
    offsmethod 4 = if  8badroff exit then
    offsmethod 5 = if 32badroff exit then
    cr ." fatal error in label system " abort ;

: CLEAROPCADRROW ( row_in_opcadr_table -- )
        FF swap opcadrlbl! ;        \ reset - ensures that every opcode is only resolved once

: (RESOLVE ( addr offset_bits -- )  \ resolves both 16 and 32b opcodes

\ **********************************\
\ this word relies on the ability   \
\ to write a 32b word to a 2 byte   \
\ aligned address!!                 \
\ **********************************\

\    over                           \ ( addr offset_bits addr )
\    offsmethod 3 = if              \ -> BL -> 32b opcode!!
\        @ or swap safe!            \ safe! necessary for ARM - safe@ is not needed
\    else
\        16b@                       \ get unresolved_opcode
\        or swap 16b!               \ write completed_opcode to memory
\    then ;

    over                            \ ( addr offset_bits addr )

    offsmethod 3 = if               \ -> BL -> 32b opcode!!
        @ or swap safe! exit        \ safe! necessary for ARM - safe@ is not needed
    then

    offsmethod 5 = if               \ methods 3 and 5 can in fact be combined
    	cr ." ITC: stack: " .s
        safe! exit
    then

    16b@                            \ get unresolved_opcode
    or swap 16b! ;                  \ write completed_opcode to memory ;

0 value templbl#
: RESOLVE ( lbl# -- )               \ for given label-nummer resolve any open opcodes
    #opcadr 0 do
        dup
        i opcadrlbl@ = if           \ label on row(i) equal to label to be resolved?
            i opcadrmtdadr@         \ get address and method from row(i) from opcadr-table
            to offsmethod           \ method to value offsmethod
            m/here over             \ ( addr m/here addr )
            calcoffset              \ ( addr offset_bits )
            (resolve
            i clearopcadrrow        \ ensure that every opcode is resolved only once
        then
    loop drop ;                     \ drop label-number

: TABLEENTRY ( lbl# -- )
    free*opcadr
    *opcadr adropcadr >r            \ calc address in table based on *opcadr_index
               r@     c!            \ label nummer as byte
    offsmethod r@  1+ c!            \ method as byte
    m/here     r> 2 +  ! ;          \ here or metahere as 32b word

: OFFSETBITS ( lbl# -- offset_bits/0 )
    dup lbladr@ 0= if               \ if label has no address yet
        tableentry 0                \ put label, method and address in opcadr-table
    else
        lbladr@ m/here calcoffset   \ otherwise calculate the offsetbits
    then ;

: OFFSETBITSLIT ( lit# -- 0 )       \ literal and ADR, never expect an address in label_table
    tableentry 0 ;                  \ -> always put label, method and address in opcadr-table

: (EOFOPC
    offsmethod 3 =
    offsmethod 5 = or if
        16bsplit eof32opc
    else
        eofopc
    then ;

: NEWLBL ( create: -- ) ( does>: opc -- completed_opc )
    create
    	newlabel# ,                 \ put consecutive numbers in new labels

    does> @							\ fetch number of label in table
    postopc? 0= if                  \ this is after an opcode_reset always the case
        dup resolve here2lbl        \ resolve if needed and put address in label_table
    else
        offsmethod 1 =              \ for literal-pool
        offsmethod 4 = or if        \ and for ADR
            offsetbitslit
        else
            offsetbits              \ for branches and ITC
        then
        or							\ label after opcode -> or with empty opcode
        (eofopc                     \ handles 16 and 32b opcodes
    then ;

\ thumb definitions
    newlbl label0:  newlbl label1:  newlbl label2:  newlbl label3:
    newlbl label4:  newlbl label5:  newlbl label6:  newlbl label7:
    newlbl lit0:    newlbl lit1:    newlbl lit2:    newlbl lit3:

newlabel# to #intllbls              \ to track where to start after a label-reset with newlabel#

: (BRH create , does> @ ppt 0 to offsmethod ( eofopc located in labels ) ;
: (BAL create , does> @ ppt 2 to offsmethod ( ditto ) ;

hex D000 (brh beq,  D100 (brh bne,  D200 (brh bcs,  D300 (brh bcc,
    D400 (brh bmi,  D500 (brh bpl,  D600 (brh bvs,  D700 (brh bvc,
    D800 (brh bhi,  D900 (brh bls,  DA00 (brh bge,  DB00 (brh blt,
    DC00 (brh bgt,  DD00 (brh ble,  E000 (bal b,
    D200 (brh bhs,  D300 (brh blo,  E000 (bal bal, \ << synonyms of bcs, bcc, b,

\ -- long branch -----
: BL,
    F000D000 ppt 3 to offsmethod ( eof32opc located in labels );

\ -- Load Register with value from literal-pool
: LIT, 4800 8 bitfld
    ppt 1 to offsmethod ;           \ fi: r1, lit, label1: - lower reg

\ -- Address to Register -----
: ADR, A000 8 bitfld
    ppt 4 to offsmethod ;           \ fi: r1, adr, label1: - lower reg

\ -- ITC address -----
: ITC 0 ppt 5 to offsmethod ;       \ fi: itc labeln: -> commas address of labeln:
decimal

forth definitions previous previous

unused - cr . .( bytes - assembler total size)
