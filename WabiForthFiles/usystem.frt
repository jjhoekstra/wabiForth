.print "definitions: usystem_p4.0      - v059ao - 04sep2022"

@ wabiForth - forth-defined part {
@  23oct19: added 5ch functionality
@  24oct19: added dirtycomma, cleancomma, updated .LcdWords
@  30oct19: nodupwarn, usehtb etal not immediate anymore
@  31oct19: added 2x 'TO+' to SEE - corrected SEE for 48b header ( added the '2 -' in SEE )
@  02nov19: added .hddnsw to SEE, adapted .linksw for this -> hidden flag now prints
@  02nov19: updated lastwords to handle HIDDEN correctly - capstype in SEE
@  03nov19: added hidden to lots of defs - previousword now NEXTWORD and returns XT and not link
@  04nov19: renamed rdnm-words to make consistant
@  07nov19: move 40 + in NEXTWORD to correct position
@  12nov19: renamed all ENDIF to THEN
@  14nov19: added 12 headerwords to SEE
@  24nov19: added WW for debugging purposes
@  27nov19: updated SEE with 4chars - deleted WW ( debugging ready ) - added HDSEE
@  05dec19: updated uartrecread & -write to uartrread & -write
@			BUG: values in RTC part without a value before it
@  12dec19: corrected bug in SEE where reg:v&reg:w goto datastack and not vv
@  19dec19: reseed rndm now 30 iso 29 due to early return of rndm - and back again
@  21dec19: bug from .avsizeword nu 48, was 40 - extended SEE - added WABIVERSION, KNOWNISSUES
@  24dec19: RNDM now late and wabi is stable!!
@  28dec19: DOTFLAGS now incl opcode_comma flag - CORE! MMU test now with own vars iso sysdbvars
@  29dec19: added WAVE stuff fi: MMUtest in startscreen for core2
@  18jan20: versionInfo adapted
@  27jan20: knownissues updated to include asminit_tst problem
@  28jan20: deleted DIRTYCOMMA, CLEANCOMMA and ref to WRDIRTFLAG
@  01feb20: changed text in pass: screen
@  03feb20: added c[ and ]c.
@  08feb20: changed dvariable to 2variable (ANSI)
@  09apr20: updated sysinfovars - removed crash of EVALUATE as known issue!
@  27apr20: added maccl et al back
@  04may20: updated KNOWNISSUES
@  06may20: updated MMU checks for core1 and core2 - added WAVE stuff
@  03jun20: renamed constants INTMAXPOS to MAXPOSINT - ditto for NEG
@  28dec20: moved wabiversion - updated DOTFLAGS inlining part
@  11jan21: bug from KNOWNISSUES (missing semicolon)
@  29jan21: slightly lowered values in (( to get better rounding
@  10feb21: new version number for wabiForth 3.0.4
@  25feb21: new version number for 3.0.6 - added i*i in SEE - BUG from --STARTSYS--: missing semicolon
@  20apr21: added 1 position to .expl
@  25apr21: added .align to FINDPRIMES as test - MAXMININT & MAXPOSINT renemaed to MAX_INT & MIN_INT
@  03may21: changed wavecyclesfree constant from 48000 to 34014 - version 3.0.7
@  04may21: version 3.1.0 - all new time-measurements in uwabiForth
@  16may21: SEC renamed to SECLONG and hidden - new codeword SEC version in uwabi
@			size words renamed and added - SEE now shows max 4000 opcodes
@  06jun21: added Assembler -> v3.2.0
@  18jun21: added missing BLX, - BX, not hidden anymore
@  22jun21: bug from (BF - missing @ after does> - added sxtb, to asm
@  23jun21: added sxtab, et al
@  24jun21: added ldrd, & strd, - deleted STRSH,
@  25jun21: added ldrex, et al - added ldrt, et al -> load complete - add parallel add and sub
@			added sel, clz, usad8, usada8,
@  26jun21: added setendle, setendbe, eret, - rfe, etal - packing etal
@  27jun21: added many multiplies - assembler now complete apart from NEON! - Wow!!
@  28jun21: added ltc, stc,
@  10aug21: added : ASSEMBLER ; to allow forgetting - moved END-SYS to before assembler
@  27aug21: memfree adapted to managed mem=16MB
@  31aug21: lastwords to lwords, awords&alwords added, ANSI dump added
@  03sep21: new version number 3.2.2
@  14sep21: new version number 3.2.3
@  04oct21: updated start screen -> steps && added 2- to mcs of core1 and core2
@  18oct21: #BUG from 'putregsbf'!! added c@scratch and c!scratch
@  09nov21: added category MISSING WORDS and put 2literal in that category
@  15nov21: added FOR...NEXT to SEE (>>.bcs)
@  16nov21: new version number 3.2.4
@  19nov21: USEHTB and NOHTB now immediate - and back (words are not used within a definition)
@  24nov21: new version number 3.2.5
@  26nov21: added FOR...NEXT stuff - added .bcc to SEE
@  30nov21: added ADDR>XT updated .XTNAME
@  15dec21: new version number 3.2.6
@  27dec21: color orange added - window 1 goes away after 10s
@  04jan22: READ2BYTE now called READ16BIT
@  11jan22: added UMIN and UMAX as temp words
@  14jan22: new version number 3.2.7
@  16jan22: changed header I2CSCAN
@  23jan22: adding new LWORDS with wid - minor version 3.2.8 - ?DO in LWORDSSL
@  25jan22: added pretty print of ORDER - very nice!
@  30jan22: changed .VOC to .VOCNAME - added .VOCS
@  06feb22: added DRAWBOX
@  10feb22: added LeCorbusier colors
@  20feb22: new medium version 3.3.0 - cfill now fill - fastfill
@  08mar22: 5CH now based on ANSI compatible WORD
@  10mar22: added ?EXIT, ?ABORT
@  14mar22: added [IF], [THEN], [ELSE], [IF, and some noForth words medium version 3.4.0
@			inclusiding case-insensitivity
@  15mar22: changed start-header and .C for 1.4 and 1.5 GHz
@  20mar22: S<> renamed to STRING<> - UPPER renamed to $>UPPER - $>LOWER added
@			CHAR and [CHAR] ow ANSI - minor version 3.4.1
@  24mar22: FPDEPTH to FDEPTH
@  29mar22: added two lines to SEE
@  04apr22: changed IMM in header of see to signed byte, changed Fcpu to fCPU
@  06apr22: minor version 3.4.2 (all definitions and create ALIGNED)
@  24apr22: CATCH, THROW -> major version 4.0.0 & 4.0.1
@  04may22: input via core2 -> medium version 4.1.0
@  21may22: ASCII character constants added
@  04sep22: added 1 opcode to SEE for THUMB -> medium version 4.2.0
@}
mycode ": --STARTSYS-- ;"
mycode "cleancache"
mycode ": .wabiversion .\" -- wabiForth32v4.2.0 -- (C) J.J. Hoekstra 2015-2022 --\" ;"

@( -- user: hex, decimal, binary )

mycode ": hex 16 base ! ;"
mycode ": decimal 10 base ! ;"
mycode ": binary 2 base ! ;"

mycode ": [hex] 16 base ! ; immediate"
mycode ": [decimal] 10 base ! ; immediate"
mycode ": [binary] 2 base ! ; immediate"

@( -- user: all )
mycode ": . ._ space ;"

mycode ": period [char] . emit ;"
mycode ": .1dec 10 /mod ._ period . ;"

mycode ": u. u._ space ;"
mycode ": d. d._ space ;"
mycode ": du. du._ space ;"
mycode ": .32hex .32hex_ space ;"
mycode ": .64hex .32hex_ .32hex_ space ;"
mycode ": .64hex_ .32hex_ .32hex_ ;"
mycode ": .32bin .32bin_ space ;"
mycode ": .64bin .32bin_ .32bin_ space ;"
mycode ": .64bin_ .32bin_ .32bin_ ;"
mycode ": .hex .hex_ space ;"
mycode ": .2d_ s>d <# # # #> type ;"

mycode ": .dec base @ >r decimal . r> base ! ;"
mycode ": u.dec base @ >r decimal u. r> base ! ;"
mycode ": d.dec base @ >r decimal d. r> base ! ;"
mycode ": du.dec base @ >r decimal du. r> base ! ;"
mycode ": .bin base @ >r binary . r> base ! ;"

mycode ": du.string <# #s #> ;"
mycode ": d.string <# #s -sign #> ;"
mycode ": rtype ( addr len r -- ) over - spaces type ;"
mycode ": d.r >r d.string r> rtype ;"
mycode ": du.r >r du.string r> rtype ;"
mycode ": .r >r s>d r> d.r ;"
mycode ": u.r >r 0 r> du.r ;"
@( value tab -- )
mycode ": .rhex base @ >r hex u.r r> base ! ;"

@( -- various stuff - all for user )
mycode ": here varhere @ ;"
mycode ": ?c ( addr -- ) c@ . ;"
mycode ": ? ( addr -- ) @ . ;"
mycode ": formalflags 0<> ;"
mycode ": .flag ( flag -- ) if .\" true\" else .\" false \" then ;"
mycode ": anykey key drop ;"

@( -- small array stuff -- )
@( example: create wkdt_a 12 cells allot )
@( numbers are put on stack in CORRECT order -> 1 2 3 gives 1 2 and 3 in array )
mycode ": loadarray ( x*n address n -- )"
mycode "	dup 0= if 2drop else"
mycode "	dup 1 = if drop ! else"
mycode "	0 swap 1- do swap over i cells + ! -1 +loop drop"
mycode "	then then ;"
mycode ": safeloadarray ( x*n address n -- )"
mycode "	dup 0= if 2drop else"
mycode "	dup 1 = if drop safe! else"
mycode "	0 swap 1- do swap over i cells + safe! -1 +loop drop"
mycode "	then then ;"
mycode ": showarray ( address n -- )"
mycode "	dup 0 <= if 2drop else"
mycode "	dup 1 = if drop @ . else"
mycode "	0 do dup i cells + @ . loop drop"
mycode "	then then ;"

@( -- date stuff -- )
mycode "create wkdt_a hidden 12 cells allot"
mycode "0 3 2 5 0 3 5 1 4 6 2 4 wkdt_a 12 loadarray"
mycode ": get_t ( 1-12 -- t ) 1- cells wkdt_a + @ ; hidden"

@( gregorian calender - Sunday=1 - needs more testing - ~96c )
mycode ": getweekday ( dd mm yyyy -- 1-7 )"
mycode "	over 3 < if 1- then ( <- year 1- if month < 3 )"
mycode "	dup 4 / swap dup 100 / swap dup 400 / swap"
mycode "	+ swap - + swap get_t + + 7 mod 1+ ;"

mycode ": leapyear? ( yyyy -- fl )"
mycode "	dup 4 mod 0<> if drop false else"
mycode "	dup 400 mod 0= if drop true else"
mycode "	dup 100 mod 0= if drop false else drop true"
mycode "	then then then ;"

@( -- time-measurements user: sec, .uptime )
@( return seconds since start system - resets to 0x0 after RELOAD/restart )
mycode ": seclong dmcs d% 1000000 ud/mod 2swap 2drop d>s ; hidden"
mycode ": .uptime ( -- ) seclong 60 /mod swap >r 60 /mod swap >r"
mycode "	._ [char] h emit r> .2d_ [char] m emit r> .2d_ [char] s emit ;"
mycode ": .uptime4boot ( -- ) seclong 60 /mod swap >r 60 /mod swap >r"
mycode "	4 .r [char] h emit r> .2d_ [char] m emit r> .2d_ [char] s emit ; hidden"

@( -- user: wait, blink )
mycode ": waitmcs ( wait_period=u -- )" @ wait n mcs - fairly accurate
mycode "	dup 0<> if dup 3 > if 1- >r dmcs begin 2dup dmcs 2swap d- r@ 0 d>"
mycode "	until 2drop r> drop else 750 * 300 - 0 do loop then else drop then ;"
mycode ": wait 1000000 waitmcs ;"
mycode ": blink 100000 waitmcs ;"
mycode ": 10ms   10000 waitmcs ;"
mycode ": ms     1000 * waitmcs  ;"

@( -- PIXEL spul user: wincursor - CHAR based cursor!! )
mycode ": wincursor ( win# -- x y ) 0 winmax 1- clip winadr> dup 100 + @ swap 104 + @ ;"
mycode ": winvisible ( n win# -- ) 0 winmax 1- clip winadr> 128 + ! ;"
mycode ": wingetsize ( win# -- x y ) winadr> dup 16 + @ swap 20 + @ ;"
mycode ": wingetink ( win# -- ink ) winadr> 72 + @ ;"
mycode ": wingetcanvas ( win# -- canvas ) winadr> 76 + @ ;"
mycode ": drawbox ( x y lenx leny win# )"
mycode "  swap 1 max 0 do 4dup rot i + -rot horline loop 4drop ;"


@( -- kleur-constantes - 37 hoofdkleuren -- )
mycode "hex"

mycode "010201 constant black"
mycode "F0F0F0 constant white"

mycode "808080 constant gray"
mycode "2F3A38 constant dgray"
mycode "121A12 constant vdgray" @ was 162016
mycode "B0B3B3 constant lgray"
mycode "C4D0D0 constant vlgray"

mycode "F00000 constant red"
mycode "500000 constant dred"
mycode "280000 constant vdred"
mycode "F08585 constant lred"
mycode "F0C0C8 constant vlred"

mycode "F08000 constant orange"
mycode "B06800 constant dorange"
mycode "603000 constant vdorange"
mycode "F0B040 constant lorange"
mycode "F0D890 constant vlorange"

mycode "F0F000 constant yellow"
mycode "909000 constant dyellow"
mycode "282800 constant vdyellow"
mycode "F0F080 constant lyellow"
mycode "F0F0D0 constant vlyellow"

mycode "0000F0 constant blue"
mycode "030660 constant dblue"
mycode "000030 constant vdblue"
mycode "1890F0 constant lblue"
mycode "80D8F0 constant vlblue"

mycode "02F004 constant green"
mycode "027004 constant dgreen"
mycode "003000 constant vdgreen"
mycode "48F03A constant lgreen"
mycode "A8F0A0 constant vlgreen"

mycode "00F0F0 constant cyan"
mycode "009898 constant dcyan"
mycode "004040 constant vdcyan"
mycode "60F0F0 constant lcyan"
mycode "A0F0F0 constant vlcyan"

mycode "F000F0 constant magenta"
mycode "500050 constant dmagenta"
mycode "280028 constant vdmagenta"
mycode "F080F0 constant lmagenta"
mycode "F0C0F0 constant vlmagenta"

@ Lize haar eigen kleur - dd 07feb2022
mycode "40E840 constant greenlo"

mycode "decimal"
mycode ": >1num swap 8 lshift or swap 16 lshift or ;"

@ the 62 Le Corbusier colors
mycode "234 219 192 >1num constant 32001_blanc"
mycode " 94  96  97 >1num constant 32010_grisfon31"
mycode "146 148 148 >1num constant 32011_gris31"
mycode "167 168 165 >1num constant 32012_grismoy"
mycode "188 187 182 >1num constant 32013_grisclr31"
mycode " 77 106 168 >1num constant 32020_bleuoutr31"
mycode "143 171 201 >1num constant 32021_outrmoy"
mycode "171 189 200 >1num constant 32022_outrclr"
mycode "182 198 206 >1num constant 32023_outrpal"
mycode "217 225 221 >1num constant 32024_outrgris"
mycode " 62 110 144 >1num constant 32030_bleuceru31"
mycode "103 157 174 >1num constant 32031_ceruvif"
mycode "138 181 186 >1num constant 32032_cerumoy"
mycode "168 196 193 >1num constant 32033_ceruclr"
mycode "198 213 204 >1num constant 32034_cerupal"
mycode "145 175 161 >1num constant 32041_vertanglclr"
mycode "190 203 183 >1num constant 32042_vertanglpal"
mycode " 62 111  66 >1num constant 32050_vertfon"
mycode "127 162  90 >1num constant 32051_vert31"
mycode "171 193 122 >1num constant 32052_vertclr"
mycode "196 211 155 >1num constant 32053_vertjauneclr"
mycode "234 207 166 >1num constant 32060_ocre"
mycode "212 108  64 >1num constant 32080_orange"
mycode "220 141 103 >1num constant 32081_orangeclr"
mycode "234 207 185 >1num constant 32082_orangepal"
mycode "155  55  56 >1num constant 32090_rouverm31"
mycode "230 205 191 >1num constant 32091_rosepal"
mycode "143  58  67 >1num constant 32100_roucar"
mycode "148  58  77 >1num constant 32101_rourub"
mycode "214 175 166 >1num constant 32102_roseclr"
mycode "139  77  62 >1num constant 32110_ocrerou"
mycode "205 152 134 >1num constant 32111_ocreroumoy"
mycode "219 190 170 >1num constant 32112_ocrerouclr"
mycode "104  68  60 >1num constant 32120_tersnnbru31"
mycode "182 123 102 >1num constant 32121_tersnnbrq"
mycode "216 178 154 >1num constant 32122_tersnnclre31"
mycode "226 203 181 >1num constant 32123_tersnnpal"
mycode " 76  66  61 >1num constant 32130_terombbru31"
mycode "183 163 146 >1num constant 32131_ombbruclre"
mycode " 90  85  80 >1num constant 32140_ombnat31"
mycode "146 138 126 >1num constant 32141_ombnatmoy"
mycode "183 172 157 >1num constant 32142_ombnatclre"
mycode "172  68  58 >1num constant 4320A_rouverm59"
mycode "234 228 215 >1num constant 4320B_blancivo"
mycode "219 163 175 >1num constant 4320C_rosevif"
mycode "116  68  56 >1num constant 4320D_tersnnbru59"
mycode " 58  59  59 >1num constant 4320E_noirivo"
mycode "184 161  54 >1num constant 4320F_vertolvvif"
mycode " 66 143 112 >1num constant 4320G_vert59"
mycode "129 134 139 >1num constant 4320H_gris59"
mycode " 64  60  58 >1num constant 4320J_terdombbru59"
mycode " 57  87 165 >1num constant 4320K_bleuoutr59"
mycode "219 176 127 >1num constant 4320L_ocrejauneclr"
mycode "116  57  59 >1num constant 4320M_lerubis"
mycode "122 167 203 >1num constant 4320N_bleuceru59"
mycode "146 150 154 >1num constant 4320O_grisclr59"
mycode "221 191 153 >1num constant 4320P_tersnnclre59"
mycode " 69  66  62 >1num constant 4320R_ombnat59"
mycode "196  94  58 >1num constant 4320S_orangevif"
mycode " 49  61 107 >1num constant 4320T_bleuoutrfon"
mycode " 96 100 106 >1num constant 4320U_grisfon59"
mycode "242 187  29 >1num constant 4320W_jaunevif"

@ reset windows for restart - only for window 1-7!
mycode ": rstwins winmax 1 do false i winvisible loop ;"
mycode "rstwins"

@ window voor banner
mycode "414 166 1 makewin 0 wingetsize drop 418 - 4 1 >winorig" @ maak window 1 voor de banner 4 pixels van re kant
mycode "lyellow 1 >wincanvas 1 winclear 2 1 >winrand dgray 1 >winink"
mycode "true 1 >winnoscroll ( true ) 1000 1 winvisible"
mycode "1 0 win#>task#"

@( -- user: none )
mycode ": memfree heredata @ varhere @ - 1024 1024 * 16 * + ;" @ this is everything between dictionary and alloted plus textmem

mycode "1 winhome"
mycode "uartcr space .\" ---------------------------------------\""
mycode "cr 3 tab .\" WABI EXTENDED FORTH V4.2.0 \" cr"
mycode "3 tab .\" (C) 2022 JJ HOEKSTRA \" cr cr"
mycode "3 tab memfree u. .\" BYTES FREE -- \""
mycode ".\" fCPU \" cpufreq 1000000 / . .\" MHz\" cr cr"

mycode "3 tab .\" Ready\" cr "
mycode "space .\" ---------------------------------------\""

@ window voor banner - nu nog experimenteel
mycode ": onderwinbanner 10 0 do 0 wincursor swap drop 7 > if leave then cr loop ; hidden"
mycode "0 0 win#>task# 1 0 uart>task# onderwinbanner 0 0 uart>task#"

@( -- user noinline, inline, dowarn, nowarning, prune, noprune, usehtb, nohtb )
mycode "0 value inlinestatus hidden "
mycode ": haltinline maxinline @ to inlinestatus 0 maxinline ! ;"
mycode ": inlineasbefore inlinestatus maxinline ! ;"
mycode ": noinline 0 maxinline ! ; immediate"
mycode ": inline 8 maxinline ! ; immediate" @ 8 is optimaal vwb inlining - meer als 9 is never_nooit_niet nuttig
mycode ": prune true optimizedd ! ; immediate" @ switches dup_drop optimization on and off
mycode ": noprune false optimizedd ! ; immediate"
mycode ": dupwarn false nowarnflag ! ;" @ switches duplicate definition warning on and off
mycode ": nodupwarn true nowarnflag ! ;"
mycode ": usehtb htb_redo true htb_flag ! ;" @ not immediate
mycode ": nohtb false htb_flag ! ;" @ not immediate
mycode ": nocompound false compoundflag ! ; immediate"
mycode ": compound true compoundflag ! ; immediate"
mycode ": nooptimize postpone noinline postpone noprune postpone nocompound ;"
mycode ": optimize postpone inline postpone prune postpone compound ;"

@( -- FOR...NEXT stuff )
mycode ": FOR_pre true to ForasDo ; immediate"
mycode ": FOR_post false to ForasDo ; immediate"

@( -- user: */, cubrt)
mycode ": */ >r m* r> s>d d/mod 2swap 3drop ;" @ >r instead of -rot and r> instead of rot saves 2 cycli
mycode ": cubrt dup >r " @ ~240c
mycode "	dup begin 2/ 2/ 2/ dup while swap 2/ 2/ swap repeat drop"
mycode "	begin r@ over / over / over + over + 3 / 2dup > while"
mycode "	nip repeat r> 2drop ;"
mycode ": reload space .\" pfff... again... \" reboot ;" @ doet een reload van wabiForth
mycode ": cold backspace .\" ? cold!? I'LL SHOW YOU COLD!!...\" cr blink syscold ;"
mycode ": bye backspace .\" ? BYE!? "
mycode "  .\" Who says you can leave?!...\" ;"
mycode ": frog backspace .\" ? FROG? I don't know nothing about no frog!!...\" ;"
mycode ": .2dec 100 /mod ._ period . ;"
mycode ": .bit if 1 . else 0 . then ;"
mycode ": .byte s>d hex <# # # #> type decimal space ;"
mycode ": .sbyte ( signed byte -- ) dup 128 < if . else"
mycode "	[hex] FFFFFF00 [decimal] or . then ;"
mycode ": .word .32hex space ;"
mycode ": noesc? key? if key 27 <> else true then ;"
mycode ": esc? key? if key 27 = else false then ;"
mycode ": ~emit dup 32 127 within if emit else drop 95 ( 126 ) emit then ;"
mycode ": emitb3 ( n -- ) 24 rshift ~emit ;"
mycode ": emitb2 ( n -- ) 16 rshift 255 and ~emit ;"
mycode ": emitb1 ( n -- )  8 rshift 255 and ~emit ;"
mycode ": emitb0 ( n -- ) 255 and ~emit ;"

@ ( -- SYSINFO primitive variables -------------------- )
mycode "sysinfovars"							@ put base-address sysinfovars on stack
mycode "    dup constant sivhandlereset"
mycode "4 + dup constant sivundefined"
mycode "4 + dup constant sivundefinedadr"
mycode "4 + dup constant sivmonitor"
mycode "4 + dup constant sivprefetchabort"
mycode "4 + dup constant sivdataabort"
mycode "4 + dup constant sivdataabortadr"
mycode "4 + dup constant sivirq"
mycode "4 + dup constant sivfiq"				@ duplicate of FIQCOUNTER
mycode "4 +		constant sivprefetchabortadr"

mycode ": .siv 10 0 do sysinfovars i cells + @ cr . loop ;"

mycode "sysdbvars"								@ base-addr sysdbvars on stack
mycode "    dup constant sivdb0"				@ temp vars for debugging-purposes
mycode "4 + dup constant sivdb1"
mycode "4 + dup constant sivdb2"
mycode "4 + dup constant sivdb3"
mycode "4 + dup constant sivdb4"
mycode "4 + dup constant sivdb5"
mycode "4 + dup constant sivdb6"
mycode "4 + dup constant sivdb7"
mycode "4 +     constant sivdb8"

mycode ": .dbvars 9 0 do cr 6 tab .\" sivdb\""
mycode "  i . i 4 * sysdbvars + @ .hex loop cr ;"

mycode "hex 1000000 constant textblocksize decimal"

@ ( -- System setup constants -------------- )
mycode "0 constant dcisw"						@ datacache - invalidate by set/way - see DCSWL1 & DCSWL1L2
mycode "1 constant dccsw"						@ datacache - clear by set/way - idem
mycode "2 constant dccisw"						@ datacache - clear/invalidate by set/way - idem

@ ( -- number constants -------------------- )
mycode "hex 80000000 constant min_int decimal"
mycode "hex 7FFFFFFF constant max_int decimal"

mycode "1 : setbits 0 do dup constant 1 lshift loop ;"
mycode "8 setbits bit0  bit1  bit2  bit3  bit4  bit5  bit6  bit7"
mycode "8 setbits bit8  bit9  bit10 bit11 bit12 bit13 bit14 bit15"
mycode "8 setbits bit16 bit17 bit18 bit19 bit20 bit21 bit22 bit23"
mycode "8 setbits bit24 bit25 bit26 bit27 bit28 bit29 bit30 bit31"
mycode "drop"

@ ( -- memory variables -------------------- )
@ Marsaglia stuff used to be here

@ ( -- I2C constants ----------------------- )
@ ( bsc0 not available to users on pi3 - bsc0 starts from 3F2050xx )
mycode "hex"
mycode "3F804000 constant bsc1_c"				@ control register BCM serial controller 1
mycode "3F804004 constant bsc1_s"				@ status register
mycode "3F804008 constant bsc1_dlen"			@ data length
mycode "3F80400C constant bsc1_a"				@ address of slave
mycode "3F804010 constant bsc1_fifo"			@ 16 byte deep - read and write data per byte from this address
mycode "3F804014 constant bsc1_div"				@ divider: 4000 voor 100kHz with a core freq of 400Mhz
mycode "3F804018 constant bsc1_del"				@ data-delay
mycode "3F80401C constant bsc1_clkt"			@ clock-stretch timeout - 0 disables timeout -> hang when no ACK!
mycode "decimal"

@ UART printing default 'ON' for task 0
mycode "0 0 uart>task#"

@ was classic Byte-SIEVE in assembly and in wabi-forth - to confirm working MMU, to check optimizations
@ new: algorithm by Terry Noyes - corrected by Stephen Brault
@ Vierte Dimension - 1987 - VD87 III/1 - page 12
mycode "cr cr 8190 sieve .\" sieve (assem):  \" 3 spaces dup ."
mycode ".\" mcs  (pass: < 900) ->  \" "
mycode "78000 swap / .1dec .\" * IBM-3033 assem\" space . cr "
mycode "8190 constant primesize hidden"
mycode "create flarray hidden primesize allot flarray constant flags hidden"
mycode "4 .align 4 allot"
mycode " : findprimes ( -- primes )"
mycode "	flags primesize 1 ( cfill ) fill 0 primesize 0 do"
mycode "		flags i + c@"
mycode "		if 3 i + i + dup i + primesize"
mycode "			< if primesize flags + over flags + i +"
mycode "				do 0 i c! dup +loop"
mycode "			then drop 1+ then"
mycode "	loop ; hidden"
mycode ": wabisieve 10 0 do findprimes drop loop ;"
mycode "mcs wabisieve mcs swap - .\" sieve (forth): \" dup 7 .r space"
mycode " .\" mcs  (pass: <2000) -> \" " @was once 6100-6200 and also <4650
mycode "1100000 swap / .1dec .\" * CRAY-1 Fortran \" findprimes . cr"

@ 5cdump converts a memory-range into 5ch lines
@ 5c| accepts a 5ch coded string and commas the values as opcodes
@ it can be used for all code-words and words without branches
mycode ": >5char ( n -- 5*n ) 5 0 do 93 u/mod loop drop ; hidden"
mycode ": .5char ( 5*n -- ) 5 0 do 33 + emit loop ; hidden"
mycode ": 5cval ( addr -- val ) 0 swap 5 0 do dup i + c@ 33 -"
mycode "	rot 93 * + swap loop drop ; hidden"
mycode ": 5chline ( addr, no -- ) dup 0<> if cr .\" 5c| \" 0 do dup i 4 * +"
mycode "	@ >5char .5char loop drop else 2drop then ; hidden"
mycode "0 value 5chcn hidden"
mycode ": 5chdump ( addr, no -- ) dup 0= if sysabort then to 5chcn begin"
mycode "	5chcn 10 > if dup 10 5chline -10 +to 5chcn 40 + false"
mycode "	else 5chcn 5chline true then until cr ;"
mycode ": 5c| 32 word count dup 0= if sysabort then 5 / 0 do dup i 5 * +"
mycode "  5cval opcode, loop drop ; immediate"
@mycode ": 5c| 32 word dup 0= if sysabort then count 5 / 0 do dup i 5 * +"
@mycode "	5cval opcode, loop drop ; immediate"

@ string-manipulations
@ ( copies incl length at start of string )
mycode ": copy$ ( *addr_src len *addr_dest -- ) 2dup ! 4 + swap cmove ;"
mycode ": add$ ( *addr_src len *addr_dest -- )"
mycode "	dup >r 2dup @ + >r dup @ + 4 + swap cmove r> r> ! ;"
mycode ": blank ( addr u -- ) bl ( cfill ) fill ;"

@( -- internal words LASTWORDS -- )
@ link in header points to previous XT!
@ next link field is thus link_in_header minus 40!
mycode ": initlw last @ ; hidden"
mycode ": nxtlnklw @ dup 0<> if 40 - then ; hidden"
@mycode ": .lnklw ( lnk -- lnk ) dup .\" 0x\" .hex ; hidden"
mycode ": (.xtname) ( xt -- ) 32 - dup 1- c@ 0 32 clip type ; hidden"
mycode ": .xtname dup 0<> if (.xtname) else"
mycode "  .\" -- not part of dictionary\" drop then ; hidden"
mycode ": .xtlw ( lnk -- lnk ) dup 40 + .\" xt: 0x\" .hex ; hidden"
mycode ": .xtcommalw ( lnk -- lnk ) dup 40 + . .\" , \" ; hidden"
mycode ": .inllw ( lnk -- lnk ) dup 4 + c@ .\" inl: \" . ; hidden"
mycode ": >1stlw ( lnk -- lnk, [char] ) dup 8 + c@ ; hidden"
mycode ": .inlcommalw ( lnk -- lnk ) dup 4 + c@ . .\" , \" ; hidden"
@mycode ": .lenlw ( lnk -- lnk ) dup 7 + c@ . ; hidden"
mycode ": .lencommalw ( lnk -- lnk ) dup 7 + c@ . .\" ,\" ; hidden"
mycode ": >typelw ( lnk -- lnk, addr-c, u ) dup 8 + dup 1- c@ ; hidden"
mycode " : lnkwid ( lnk -- lnk wid ) dup 6 + c@ ; hidden"
mycode "0 value tempwidlw"
mycode ": cr?lw ( len -- len ) dup out @ + 78 > if"
mycode "	cr else dup 0 <> if space space then then ; hidden"
mycode ": .namelw ( lnk -- lnk ) >typelw cr?lw type ; hidden"
mycode ": .namecommalw ( lnk -- lnk ) >typelw space .\" '\" type .\" ' ,\" ; hidden"

@( -- print HashTable user: .htb )
mycode ": linkaddr>pos dup 0<> if htb_addrlnk - 12 / else -1 then ; hidden"
mycode ": .link dup 0<> if htb_addrlnk - 12 / 7 .r else drop space .\" nolink\" then ; hidden"
mycode ": .cellcont	2 spaces dup @ .link dup 4 + @ 10 hex u.r decimal"
mycode "	8 + @ dup 9 hex .r decimal 2 spaces .xtname ; hidden"
mycode ": .lnk_cell cr 11 spaces dup linkaddr>pos 6 .r .cellcont ; hidden"
mycode ": .htb_cell cr"
mycode "	8 spaces .\" ep:\" i 6 .r .cellcont ; hidden"
mycode ": followlinks begin @ dup 0<> noesc? and"
mycode "	while dup .lnk_cell repeat drop ; hidden"
mycode ": .filled_htb_cell dup 4 + @ 0<> if swap 1+ swap"
mycode "	dup .htb_cell followlinks else drop then ; hidden"
mycode ": .htb 0 htb_entlen 0 do i 12 * htb_addrent + .filled_htb_cell"
mycode "	loop cr .\" filled htb_cells entry_pool: \" . ;"

@( -- : user: all )
@( a few sizes 'ter leering ende vermaeck' - updated and documented )
mycode ": word# 0 initlw begin swap 1+ swap nxtlnklw dup 0= until drop ;"
mycode ": endboot varendboot @ ;"
mycode ": szsysdata startdict 32768 - ;"
mycode ": szbasedict startfree startdict - ;"
mycode ": sztooldict endboot startfree - ;"
mycode ": szuserdict here endboot - ;"
mycode ": szsystotal endboot 32768 - ;"
mycode ": szdict here startdict - ;"
mycode ": .avsizeword szbasedict sztooldict + szuserdict + "
mycode "	word# 48 * - 10 * word# / 4 / .1dec .\" opcodes\" ;" @ av size in opcodes van een word
mycode ": unused heredata @ here - ;" @ presently 15.6 words/definition

@( -- : user: addr>xt ( addr -- 0/xt )
mycode " 0 value xtvalid?"
mycode " : (checkxt) 40 + swap over - 0> to xtvalid? ; hidden"
mycode " : addr>xt dup here >= if drop 0 false to xtvalid? else dup initlw"
mycode "  8 - >= if initlw (checkxt) else initlw begin nxtlnklw 2dup 8 - >="
mycode "  until dup 0= if nip false to xtvalid? else (checkxt) then then then ;"

@( -- : user: abort, ?abort ?exit
mycode ": abort .\" -> aborted \" sysabort ;"
mycode ": ?abort if r13@ @ addr>xt dup .\" -> abort in: \" .xtname"
mycode "  space .\" with XT: 0x\" .hex sysabort then ;"
mycode ": ?exit postpone if postpone exit postpone then ; immediate"

@( -- wordlist stuff - vocabulary for user )
mycode "create widlist 256 cells allot ;"
mycode ": clearwidlist widlist 256 0 fastfill ; clearwidlist"
mycode ": vocabulary create here wordlist dup ,	4* widlist + !"
mycode "  does> @ >r get-order nip r> swap set-order ;"
mycode ": .vocname ( wid -- ) dup 0= if .\" FORTH\" drop"
mycode "  else 4* widlist + @ addr>xt .xtname then ;"
mycode ": order cr .\" definitions : \" cr get-current 8 spaces .vocname cr"
mycode "  .\" search order: \" 1 *so_table @ do cr i search_order_table"
mycode "  + c@ 8 spaces .vocname -1 +loop ;"
mycode ": .vocs cr 8 spaces .\" wid name\" cr 8 spaces .\" --- ------- \""
mycode "  word_list_count @ 1+ 1 ?do cr i 11 .r space i .vocname loop"
mycode "  cr 0 11 .r space .\" FORTH\" cr 8 spaces .\" --- ------- \" ;"

@( -- user: words, wordsl, lwords, lastwordsl, lastwordscomma, #wordslw )
@( now value instead of variable #wordslw - lwords updated - now documented )
mycode "  50 value #wordslw   0 value char>"
mycode ": (lwords) ( wid -- ) cr .\" wid: \" to tempwidlw tempwidlw .vocname"
mycode "  4 spaces 0 initlw #wordslw 0 ?do dup 1 - c@ 0= if"
mycode "  lnkwid tempwidlw = if .namelw swap 1+ swap then"
mycode "  then nxtlnklw dup 0= if leave then loop 2drop ;"
mycode ": lwords 1 *so_table @ dup 0= abort\" fatal error in wordlist\""
mycode "  do search_order_table i + c@ (lwords) -1 +loop ;"
mycode ": alwords ( -- ) postpone [char] >caps to char> 0	initlw #wordslw 0<> if"
mycode "	[char] : emit 2500000 0 do dup 1 - c@ 0= if >1stlw >caps char> = if"
mycode "	( cr over 1+ 5 .r ) .namelw swap 1+ swap then then over #wordslw u> if"
mycode "	leave then nxtlnklw dup 0= if leave then loop else space"
mycode "	.\" set value #wordslw to 1 or larger \" then 2drop ; hidden"
mycode ": lwordsl ( -- ) cr initlw #wordslw 0 ?do .xtlw .inllw"
mycode "  .namelw cr nxtlnklw dup 0= if leave then loop drop ;"
mycode ": lwordscomma ( -- ) initlw #wordslw 0 ?do .xtcommalw .inlcommalw"
mycode "  .lencommalw .namecommalw cr nxtlnklw dup 0= if"
mycode "  leave then loop drop ;"
mycode ": words #wordslw >r -1 to #wordslw lwords r> to #wordslw"
mycode "	cr .\" ( \" word# . .\" words )\" ;"
mycode ": awords #wordslw >r -1 to #wordslw alwords r> to #wordslw ; "
mycode ": wordsl #wordslw >r -1 to #wordslw lwordsl r> to #wordslw ;"
mycode ": wordscomma #wordslw >r -1 to #wordslw lwordscomma r> to #wordslw ;"


@ showword takes an xt and no of opcodes as input and then shows the content of a definition
@ ' dup 4 showword --> shows the def of dup
@ : foo [ ' dup makelit ] 4 showword ; shows the def of dup from a definition
@ : foo [ ' dup ] literal 4 showword ; also
@ ' dup : foo literal 4 showword ; also
mycode ": swinpre 2 spaces .\" | \" ; hidden"
mycode ": swljpre 1 spaces .\" << \" ; hidden"
mycode "hex"
mycode ": .expl ( opcode -- ) "
mycode "	dup E52A8004 = if swinpre .\" DO: push do-limit on R \" then "
mycode "	dup E52A7004 = if swinpre .\" DO: push do-index on R \" then "
mycode "	dup E1A07005 = if swinpre .\" DO: copy top to do-index \" then "
mycode "	dup E1A08005 = if swinpre .\" 0DO: copy top to do-limit \" then "
mycode "	dup E3A07000 = if swinpre .\" 0DO: put 0 in do-index \" then "
mycode "	dup E4998004 = if swinpre .\" DO: pop 2nd to do-limit - dts 1 pos down \" then "
mycode "	dup E320F000 = if swinpre .\" nop \" then "
mycode "	dup E58B5000 = if swinpre .\" TO: store top in \[reg:v\] \" then "
mycode "	dup E58BC000 = if swinpre .\" TO+: store reg:w in \[reg:v\] \" then "
mycode "	dup E08CC005 = if swinpre .\" TO+: add top to reg:w \" then "
mycode "	dup E1A05FC5 = if swinpre .\" arithm. shift top right with 31 bits \" then "
mycode "	dup F57FF05F = if swinpre .\" DMB =memory-barrier \" then "
mycode "	dup F57FF04F = if swinpre .\" DSB =data-barrier \" then "
mycode "	dup F57FF06F = if swinpre .\" ISB =instruction-barrier \" then "
mycode "	dup E5955000 = if swinpre .\" FETCH: word from \[top\] to top \" then "
mycode "	dup E5D55000 = if swinpre .\" CFETCH: char from \[top\] to top \" then "
mycode "	dup E7D55007 = if swinpre .\" IPLUSCFETCH: char from \[top + i\] to top \" then "
mycode "	dup E7D5500C = if swinpre .\" PLUSCFETCH: char from \[top + reg:w\] to top \" then "
mycode "    dup E8BD99DF = if swinpre .\" maxnext \" then "
mycode "    dup E92D59DF = if swinpre .\" maxprolog \" then "
mycode "	dup E8BD8000 = if swinpre .\" fastnext lr only \" then "
mycode "	dup E92D4000 = if swinpre .\" fastprolog lr only \" then "
mycode "	dup E8BD8001 = if swinpre .\" fastnext lr & r0 \" then "
mycode "	dup E92D4001 = if swinpre .\" fastprolog lr & r0 \" then "
mycode "	dup E8BD800F = if swinpre .\" next \" then "
mycode "	dup E92D400F = if swinpre .\" prolog \" then "
mycode "	dup FA000000 = if swinpre .\" switch to THUMB \" then "
mycode "	dup E12FFF1E = if swinpre .\" codenext \" then "
mycode "	dup E4995004 = if swinpre .\" DROP: 2nd to top - dts 1 pos down \" then "
mycode "	dup E5995004 = if swinpre .\" copy 3rd to top \" then "
mycode "	dup E2899008 = if swinpre .\" 2DROP: lower stackpointer with 2 pos \" then "
mycode "	dup E5995008 = if swinpre .\" 3DROP: update top from 4th \" then "
mycode "	dup E289900C = if swinpre .\" 3DROP: lower stackpointer with 3 pos \" then "
mycode "	dup E599500C = if swinpre .\" 4DROP: update top from 5th \" then "
mycode "	dup E2899010 = if swinpre .\" 4DROP: lower stackpointer with 4 pos \" then "
mycode "	dup E5995010 = if swinpre .\" 5DROP: update top from 6th \" then "
mycode "	dup E2899014 = if swinpre .\" 5DROP: lower stackpointer with 5 pos \" then "
mycode "	dup E59AE000 = if swinpre .\" LEAVE: move addr from R to linkreg \" then "
mycode "	dup E5295004 = if swinpre .\" DUP: dts 1 pos up - copy top to 2nd \" then "
mycode "	dup E5895000 = if swinpre .\" copy top to 2nd \" then "
mycode "	dup E529B004 = if swinpre .\" dts 1 pos up - copy reg:v to 2nd \" then "
mycode "	dup E1A05007 = if swinpre .\" copy i to top \" then "
mycode "	dup E2875001 = if swinpre .\" copy i+1 to top \" then "
mycode "	dup E2875004 = if swinpre .\" copy i+4 to top \" then "
mycode "	dup E5297004 = if swinpre .\" dts 1 pos up - copy i to 2nd \" then " @ ='i swap'
mycode "	dup E0855007 = if swinpre .\" add i to top \" then "
mycode "	dup E1A05107 = if swinpre .\" copy i*4 to top \" then "
mycode "	dup E1A05087 = if swinpre .\" copy i*2 to top \" then "
mycode "	dup E0050797 = if swinpre .\" copy i*i to top \" then "
mycode "	dup E7955107 = if swinpre .\" fetch word from \[i*4+top\] \" then "
mycode "	dup E59A5004 = if swinpre .\" push J on stack \" then "
mycode "	dup E59A5010 = if swinpre .\" push K on stack \" then "
mycode "	dup E49A5004 = if swinpre .\" push R on stack \" then "
mycode "	dup E52DE004 = if swinpre .\" push reg:lr on sys_stack \" then "
mycode "	dup E49DE004 = if swinpre .\" pop reg:lr from sys_stack \" then "
mycode "    dup E52A5004 = if swinpre .\" push top on R \" then "
mycode "    dup E52AC004 = if swinpre .\" push reg:w on R \" then "
mycode "    dup E59A5000 = if swinpre .\" copy R-stack to stack \" then "
mycode "    dup E52AB004 = if swinpre .\" INITLEAVE: push addr in reg:v on R \" then "
mycode "	dup E28AA004 = if swinpre .\" drop value from R \" then "
mycode "	dup E28AA008 = if swinpre .\" drop 2 values from R \" then "
mycode "	dup E59A7004 = if swinpre .\" UNLOOP: restore loop-index from R \" then "
mycode "	dup E59A8008 = if swinpre .\" UNLOOP: restore loop-limit from R \" then "
mycode "	dup E28AA00C = if swinpre .\" UNLOOP: reset pointer of R \" then "
mycode "	dup E2877001 = if swinpre .\" LOOP: add 1 to loop-index \" then "
mycode "	dup E1570008 = if swinpre .\" LOOP: check if loop ready ( i <= limit ) \" then "
mycode "	dup E2577001 = if swinpre .\" NEXT: sub 1 from loop-index - set flags \" then "
mycode "	dup E499C004 = if swinpre .\" 2nd to reg:w - dts 1 pos down \" then "
mycode "	dup E8B91800 = if swinpre .\" 3rd to reg:w - 2nd to reg:v - dts 2 down \" then "
mycode "	dup E529C004 = if swinpre .\" dts 1 pos up - copy reg:w to 2nd \" then "
mycode "	dup E085500C = if swinpre .\" ADD reg:w to top\" then "
mycode "	dup E085500B = if swinpre .\" ADD reg:v to top \" then "
mycode "	dup E08BB005 = if swinpre .\" ADD top to reg:v \" then "
mycode "	dup E08C500C = if swinpre .\" ADD top = reg:w + reg:w\" then "
mycode "	dup E29CC001 = if swinpre .\" ADD 0x1 to reg:w - set flags \" then "
mycode "	dup E045500C = if swinpre .\" sub top = reg:w from top \" then "
mycode "	dup E04BB00C = if swinpre .\" sub top = reg:w from reg:v \" then "
mycode "	dup E2555001 = if swinpre .\" top = top minus 0x1 and set flags \" then "
mycode "	dup E0C55005 = if swinpre .\" top = top from top minus carry \" then "
mycode "	dup E2755000 = if swinpre .\" top = 0x0 minus top & set flags \" then "
mycode "	dup E300C00C = if swinpre .\" LIT: load 12 into reg:w \" then "
mycode "	dup E3A0500C = if swinpre .\" lit16: top =12 =0x0C \" then "
mycode "	dup E300C000 = if swinpre .\" LIT: load 0 into reg:w \" then "
mycode "	dup E2855001 = if swinpre .\" top=top+1 \" then "
mycode "	dup E2855002 = if swinpre .\" top=top+2 \" then "
mycode "	dup E2855003 = if swinpre .\" top=top+3 \" then "
mycode "	dup E2855004 = if swinpre .\" top=top+4 \" then "
mycode "	dup E1A05145 = if swinpre .\" top=top/4 \" then "
mycode "	dup E2855008 = if swinpre .\" top=top+8 \" then "
mycode "	dup E28CC004 = if swinpre .\" reg:w=reg:w+4 \" then "
mycode "	dup E24CC004 = if swinpre .\" reg:w=reg:w-4 \" then "
mycode "	dup E28CC001 = if swinpre .\" reg:w=reg:w+1 \" then "
mycode "	dup E24CC001 = if swinpre .\" reg:w=reg:w-1 \" then "
mycode "	dup E3A05000 = if swinpre .\" top =0 =0x0\" then "
mycode "	dup E3A05001 = if swinpre .\" top =1 =0x1\" then "
mycode "	dup E3A05002 = if swinpre .\" top =2 =0x2\" then "
mycode "	dup E3A05003 = if swinpre .\" top =3 =0x3\" then "
mycode "	dup E3A05004 = if swinpre .\" top =4 =0x4\" then "
mycode "	dup E3A05008 = if swinpre .\" top =8 =0x8\" then "
mycode "	dup E04C5005 = if swinpre .\" SUBTRACT: top from reg:w \" then "
mycode "	dup E2455001 = if swinpre .\" 1- \" then "
mycode "	dup E2455002 = if swinpre .\" 2- \" then "
mycode "	dup E2455003 = if swinpre .\" 3- \" then "
mycode "	dup E2455004 = if swinpre .\" 4- \" then "
mycode "	dup E2455008 = if swinpre .\" 8- \" then "
mycode "	dup E1A05085 = if swinpre .\" 2* \" then "
mycode "	dup E1A05105 = if swinpre .\" 4* \" then "
mycode "	dup E1A05185 = if swinpre .\" 8* \" then "
mycode "	dup E1E05005 = if swinpre .\" INVERT: bits of top (=bitwise NOT) \" then "
mycode "	dup E185500C = if swinpre .\" OR: top with reg:w \" then "
mycode "	dup E005500C = if swinpre .\" AND: top with reg:w \" then "
mycode "	dup E2055007 = if swinpre .\" top AND 7 \" then "
mycode "	dup E025500C = if swinpre .\" XOR: top with reg:w \" then "
mycode "	dup E03C5005 = if swinpre .\" XORs: top with reg:w and set flags \" then "
mycode "	dup E2899004 = if swinpre .\" NIP: discard 2nd\" then "
mycode "	dup E7995105 = if swinpre .\" PICK\" then "
mycode "	dup E2655000 = if swinpre .\" NEGATE\" then "
mycode "	dup B2655000 = if swinpre .\" ?NEGATE\" then "
mycode "	dup E1B0C005 = if swinpre .\" ABS: mov top to reg:w and set flags \" then "
mycode "	dup B26C5000 = if swinpre .\" ABS: on cond:neg top = 0 - reg:w \" then "
mycode "	dup E8990820 = if swinpre .\" copy top and reg:v to datastack\" then "
@ the following was .\" copy datastack to reg:v and reg:w \" <- error
mycode "	dup E8891800 = if swinpre .\" copy reg:v and reg:w to datastack\" then "
mycode "	dup E005059C = if swinpre .\" top = top times reg:w \" then "
mycode "	dup E0050C95 = if swinpre .\" top = top times reg:w \" then "
mycode "	dup E0050B95 = if swinpre .\" top = top times reg:v \" then "
mycode "	dup 106BBC95 = if swinpre .\" if not 0x0: reg:v = top minus (reg:v times reg:w) \" then "
mycode "	dup E715F51C = if swinpre .\" DIVIDE: reg:w by top (signed) \" then "
mycode "	dup E735F51C = if swinpre .\" DIVIDE: reg:w by top (unsigned) \" then "
mycode "	dup E715FC1B = if swinpre .\" top = divide reg:v by reg:w (signed) \" then "
mycode "	dup E065C59B = if swinpre .\" MOD: top = mod(reg:w divided by top) \" then "
mycode "	dup EEB15BC5 = if swinpre .\" SQRT: ftop=sqrt(ftop) 64b \" then "
mycode "	dup E0050595 = if swinpre .\" DUP*: top=top*top \" then "
mycode "	dup E1A0551C = if swinpre .\" LSHIFT: top is reg:w shifted left with top \" then "
mycode "	dup E1A0553C = if swinpre .\" RSHIFT: top is reg:w shifted right with top \" then "
mycode "	dup E10C5045 = if swinpre .\" 32b CRC with byte: crc32b top, reg:w, top \" then "
mycode "	dup E14C5045 = if swinpre .\" 32b CRC with word: crc32w top, reg:w, top \" then "
mycode "	dup E14B5045 = if swinpre .\" 32b CRC with word: crc32w top, reg:v, top \" then "
mycode "	dup EE045A10 = if swinpre .\" copy top to ftop\" then "
mycode "	dup EE145A10 = if swinpre .\" copy ftop to top\" then "
mycode "	dup EEB85B44 = if swinpre .\" 32b integer -> 64b float\" then "
mycode "	dup EEBC4BC5 = if swinpre .\" 64b float -> 32b integer\" then "
mycode "	dup ED864A00 = if swinpre .\" fdup: ftop to 2nd\" then "
mycode "	dup ED964A00 = if swinpre .\" fdrop: ftop from 2nd\" then "
mycode "	dup E2466004 = if swinpre .\" fps-pointer 1 pos higher\" then "
mycode "	dup E2866004 = if swinpre .\" fps-pointer 1 pos lower\" then "
mycode "	dup E585C000 = if swinpre .\" STORE: reg:w to \[top\] \" then "
mycode "	dup E59BC000 = if swinpre .\" load reg:w from \[reg:v\] \" then "
mycode "	dup E59CB000 = if swinpre .\" load reg:v from \[reg:w\] \" then "
mycode "	dup E58C5000 = if swinpre .\" copy top to \[reg:w\] \" then "
mycode "	dup E4D9C004 = if swinpre .\" copy byte in 2nd to reg:w \" then "
mycode "	dup E5C5C000 = if swinpre .\" CSTORE: byte in reg:w to \[top\] \" then "
mycode "	dup E5CBC000 = if swinpre .\" store byte in reg:w to \[reg:v\] \" then "
mycode "	dup E1A0C005 = if swinpre .\" copy reg:top to reg:w \" then "
mycode "	dup E1A0B005 = if swinpre .\" copy reg:top to reg:v \" then "
mycode "	dup E1A00005 = if swinpre .\" copy reg:top to reg:0 \" then "
mycode "	dup E5995000 = if swinpre .\" copy 2nd to top \" then "
mycode "	dup E589C000 = if swinpre .\" copy reg:w to 2nd \" then "
mycode "	dup 1589B000 = if swinpre .\" if not 0x0: copy reg:v to 2nd \" then "
mycode "	dup 05895000 = if swinpre .\" if 0x0: copy top to 2nd \" then "
mycode "	dup E599C000 = if swinpre .\" copy 2nd to reg:w \" then "
mycode "	dup E599B000 = if swinpre .\" copy 2nd to reg:v \" then "
mycode "	dup E1A0B00E = if swinpre .\" copy r14 to reg:v \" then "
mycode "	dup E1A0500B = if swinpre .\" copy reg:v to top \" then "
mycode "	dup E3550000 = if swinpre .\" IF: compare top with 0x0 \" then "
mycode "	dup E3550001 = if swinpre .\" IF: compare top with 0x1 \" then "
mycode "	dup E15C0005 = if swinpre .\" CMP: compare reg:w with top \" then "
mycode "	dup E15B0005 = if swinpre .\" CMP: compare reg:v with top \" then "
mycode "	dup E35C0000 = if swinpre .\" CMP0: compare reg:w with 0x0 \" then "
mycode "	dup E35C000A = if swinpre .\" CMP10: compare reg:w with #10 \" then "
mycode "	dup 13A05000 = if swinpre .\" FALSE to top on not equal \" then "
mycode "	dup 23A05000 = if swinpre .\" FALSE to top on unsigned higher or same \" then "
mycode "	dup D3A05000 = if swinpre .\" FALSE to top on signed less or equal \" then "
mycode "	dup A3A05000 = if swinpre .\" FALSE to top on signed greater or equal \" then "
mycode "	dup C3A05000 = if swinpre .\" FALSE to top on signed greater \" then "
mycode "	dup 33E05000 = if swinpre .\" TRUE to top on unsigned lower \" then "
mycode "	dup 13E05000 = if swinpre .\" TRUE to top on not equal \" then "
mycode "	dup 03E05000 = if swinpre .\" TRUE to top on equal \" then "
mycode "	dup C3E05000 = if swinpre .\" TRUE to top on signed greater than \" then "
mycode "	dup B3E05000 = if swinpre .\" TRUE to top on signed less than \" then "
mycode "	dup D3E05000 = if swinpre .\" TRUE to top on signed less or equal than \" then "
mycode "	dup E3E05000 = if swinpre .\" TRUE to top \" then "
mycode "	dup E12FFF1B = if swinpre .\" jump to address in reg:v \" then "
mycode "	dup E12FFF3B = if swljpre .\" long jump linked \" then "
mycode "	dup E12FFF1C = if swinpre .\" jump to address in reg:w \" then "
mycode "	dup E12FFF3C = if swinpre .\" long jump linked to addres in reg:w \" then "
mycode "	dup E12FFF30 = if swinpre .\" long jump linked to addres in reg:0 \" then "

mycode "	drop ; hidden"
mycode "decimal"

mycode "0 value addlit16 hidden"
mycode "0 value longjumplit hidden"
mycode ": getjump ( opc -- jump ) 8 lshift 64 / 8 + ; hidden"
mycode ": getlitopcode ( opc -- 16bit opc )"
mycode "		12 rshift dup 15 and swap 8 rshift 4 lshift + ; hidden"
mycode ": getlitval ( opcode -- lit16 ) dup [hex] fff and swap 000f0000 [decimal]"
mycode "	and 4 rshift + ; hidden"
mycode ": .lits ( opc -- ) dup getlitopcode [hex] E305 [decimal] = if"
mycode "	dup getlitval dup to addlit16"
mycode "	swinpre .\" lit16: top =\" dup . .\" =0x\" .hex then"
mycode "	dup getlitopcode [hex] E345 [decimal] = if"
mycode "	getlitval 16 lshift addlit16 + 0 to addlit16"
mycode "	swinpre .\" lit32: top =\" dup . .\" =0x\" .hex"
mycode "	else drop then ; hidden"
mycode ": .vlits ( opc -- ) dup getlitopcode [hex] E30B [decimal] = if"
mycode "	dup getlitval dup to addlit16"
mycode "	swinpre .\" lit16: reg:v =\" dup . .\" =0x\" .hex then"
mycode "	dup getlitopcode [hex] E34B [decimal] = if"
mycode "	getlitval 16 lshift addlit16 + dup to longjumplit 0 to addlit16"
mycode "	swinpre .\" lit32: reg:v =\" dup . .\" =0x\" .hex"
mycode "	else drop then ; hidden"
mycode ": .beq ( addr, opc -- ) dup 24 rshift 10 = if getjump +"
mycode "		swinpre .\" on cond:equal jump to 0x\" .hex"
mycode "		else 2drop then ; hidden"
mycode ": .bne ( addr, opc -- ) dup 24 rshift 26 = if getjump +"
mycode "		swinpre .\" on cond:notequal jump to 0x\" .hex"
mycode "		else 2drop then ; hidden"
mycode ": .bcs ( addr, opc -- ) dup 24 rshift 42 = if getjump +"
mycode "		swinpre .\" on cond:carryset jump to 0x\" .hex"
mycode "		else 2drop then ; hidden"
mycode ": .bcc ( addr, opc -- ) dup 24 rshift 58 = if getjump +"
mycode "		swinpre .\" on cond:carryclear jump to 0x\" .hex"
mycode "		else 2drop then ; hidden"
mycode ": .bhi ( addr, opc -- ) dup 24 rshift 138 = if getjump +"
mycode "		swinpre .\" on cond:uns higher to 0x\" .hex .\" (f.i.: +LOOP)\" "
mycode "		else 2drop then ; hidden"
mycode ": .branch ( addr, opc -- ) dup 24 rshift 234 = if getjump +"
mycode "		swinpre .\" branch to 0x\" .hex"
mycode "		else 2drop then ; hidden"
mycode ": .copro ( opc -- ) dup dup 24 rshift 238 = "
mycode "		swap 28 rshift 15 = or"
mycode "		if drop swinpre .\" co-proc\""
mycode "		else drop then ; hidden"
mycode ": .namext ( xt -- ) 32 - dup 1- c@ type ; hidden"
mycode ": .hddnsw ( xt -- )  1 - sc@ .\" hid:\" . ; hidden"
mycode ": .linksw ( xt -- )  @ .\" lnk:\" .32hex ; hidden"
mycode ": .inlsw  ( xt -- )  4 + c@ .\" inl:\" . ; hidden"
mycode ": .immsw  ( xt -- )  5 + sc@ .\" imm:\" . ; hidden"
mycode ": .widsw  ( xt -- )  6 + c@ .\" wid: \" .vocname ; hidden"
mycode ": .lensw  ( xt -- )  7 + 3 spaces c@ .\" len:\" . ; hidden"
mycode ": .namesw ( xt -- )  8 + dup 1- c@ .\" name:\" capstype ; hidden"
mycode ": .forthword ( addr, opc -- ) dup 24 rshift 235 = if getjump + .namext"
mycode "		else 2drop then ; hidden"
mycode ": .wordlongjump ( addr, opc -- ) swap drop [hex] E12FFF3B [decimal] = if"
mycode "		longjumplit .namext then ; hidden"
mycode ": 4chars ( n -- ) space dup emitB3 dup emitB2 dup emitB1 emitB0 2 spaces ;"
mycode ": .opssw ( addr, #ops -- ) 0 do i 1+ 4 .r space"
mycode "	dup dup .32hex space @ dup dup .32hex 4chars space"
mycode "	dup .vlits dup .lits"
mycode "	2dup .branch 2dup .beq 2dup .bne 2dup .bhi 2dup .bcs 2dup .bcc"
mycode "	dup .copro"
mycode "	2dup .forthword 2dup .wordlongjump .expl cr esc? if leave then 4 + loop drop ; hidden"
mycode ": .hdrwrds ( xt -- ) 48 - 12 0 do cr i 4 .r space dup .32hex"
mycode "	space dup @ dup .32hex 4chars 4 + loop drop ; hidden"
mycode ": showword ( xt, #ops -- ) >r 40 - cr dup .namesw dup .lensw dup .linksw"
mycode "	dup .hddnsw dup .inlsw dup .immsw dup .widsw cr 40 + r> .opssw ;"
mycode "0 value oldlink hidden"
mycode ": nextword ( xt -- xt ) here 8 + to oldlink initlw 2500000 0 do dup 0= if"
mycode "	2drop 0 leave then 2dup 40 + = if 2drop oldlink 40 + leave then" @ 40 + <new - returns xt that way
mycode "	2dup > if 2drop 0 leave then dup to oldlink ( xt, link )"
mycode "	nxtlnklw ( xt, nextlink ) loop ;"
mycode ": see ' dup dup nextword dup 0 <> if swap - 4 / 12 - 1 4000 clip showword" @ 12 was 2
mycode "	else 3drop cr .\" UNKNOWN error in SEE\" then ;"
mycode ": hdsee ' dup dup nextword dup 0 <> if swap - 4 / 12 - over .hdrwrds"
mycode "	1 4000 clip showword"
mycode "	else 3drop cr .\" UNKNOWN error in SEE\" then ;"

@( -- user: .compoundtable )
mycode " 1 value cmp1st hidden 1 value cmp2nd hidden"
mycode " : showaltxt ( addr+4 -- ) @ begin dup @ dup  while 18 tab"
mycode " 	cmp2nd 3 .r 2 spaces .xtname space 36 tab dup 4 + @"
mycode "	.xtname space cr cmp2nd 1+ to cmp2nd 8 + repeat 2drop ; hidden"
mycode " : .compoundtable comp_table 1 to cmp1st 1 to cmp2nd"
mycode "	cr .\" ------- 1st xt ------- 2nd xt ----- alt xt -------\" cr"
mycode "	begin dup @ dup while 3 tab cmp1st 3 .r 2 spaces .xtname"
mycode "	dup 4 + showaltxt cr cmp1st 1+ to cmp1st 8 + repeat"
mycode "    .\" --------------------------------------------------\""
mycode "	2drop ;"

@( -- user: meetdummy (x4) .c1 )
mycode ": meetdummy ; ( pi4:4.3c pi3b+:6c pi3:7c met short prolog - was 9c )"
mycode ": meet2dummy meetdummy ; ( pi4: 8.6c pi3b+:12c pi3:14c - was 17c )"
mycode ": meetdummy4 4 4 2drop ; ( pi4: 14c pi3b+10c! pi3:9c - was 10c - 4 4 2drop duurt 3c! -> 3+9=12c verwacht )"
mycode ": meetdummy2_4 meetdummy4 ; ( pi4: 16.2c pi3b+:17c! pi3:15c - was 19c ) "

@( het volgende voorbeeld meet de tijd van 4 drop nl. 2500 mcs => 3 cycles )
@( : meet (( do 4 drop loop )) ; )
@( .c1 print the number of cycles for the activities in the do ... loop )
@mycode ": .c1 ( mcs -- )"
@mycode "	1341 - ( sub 2 cycli +5 c general overhead from the do loop )" @ ouw
@mycode "	1434 - ( sub 2 cycli +5 c general overhead from the do loop )" @ reg
@mycode "	dup space . .\" mcs ->\" ( prints the received time )"
@mycode "    35 + ( add half of a tenth cycle for rounding )" @ ouw
@mycode "	14 * 1000 / ( nu cycli * 10 )" @ ouw
@mycode "    30 + ( add half of a tenth cycle for rounding )" @ reg
@mycode "	15 * 1000 / ( nu cycli * 10 )" @ reg
@mycode "	10 /mod ( -- hele, tiende cycli )"
@mycode "	space ._ period . .\" cycles\" ; hidden"

mycode ": .c1 ( mcs -- )"
mycode "	cpufreq 1400000000 = if 1437 - dup space . .\" mcs ->\" 37 + 14"
mycode "	else 1342 - dup space . .\" mcs ->\" 32 + 15"
mycode "	then * 1000 / 10 /mod space ._ period . .\" cycles\" ; hidden"
mycode " : t[ dmcs ;"
mycode " : ]t. dmcs 2swap d- space d. .\" mcs\" ;"
mycode " : c[ cpucycles ;"
mycode " : ]c. cpucycles 2swap d- d% 10 d- space d. .\" cycles\" ;"
mycode ": (( postpone mcs postpone 1000000 postpone 0 postpone do  ; immediate"
mycode ": )) postpone loop postpone mcs postpone swap postpone - postpone .c1 ; immediate"

@( -- user: rndmseed, rndmreset, .rndmseed, rndmjump )
mycode ": rndmseed ( u u -- ) 2dup 2dup 2dup rndmtoseed" @ ( seed with 2 words )
mycode "	30 0 do rndm32 drop loop ;" @ 30c
mycode ": rndmreset ( -- ) 37 124173 rndmseed ;"
mycode ": randomize 8 0 do trng32 loop rndmtoseed ;"
mycode ": .rndmseed rndmgetseed .\" 3:\" .64hex .\" 2:\" .64hex"
mycode "	.\" 1:\" .64hex .\" 0:\" .64hex ;"
mycode "rndmreset"
mycode "hex create xishojmptbl hidden" @ this table is for the jump function - see wabiForth manual
mycode "3cfd0aba , 180ec6d3 , f0c9392c , d5a61266 ,"
mycode "e03fc9aa , a9582618 , 29b1661c , 39abdc45 , decimal"
mycode "2variable s0 hidden 2variable s1 hidden 2variable s2 hidden 2variable s3 hidden"
mycode ": rndmresetvars d% 0 s0 2! d% 0 s1 2! d% 0 s2 2! d% 0 s3 2! ; hidden"
mycode ": rndmdoxors rndmgetseed s3 2@ dxor s3 2!"
mycode "	s2 2@ dxor s2 2! s1 2@ dxor s1 2! s0 2@ dxor s0 2! ; hidden"
mycode ": rndmgopastbits ( n -- ) 32 0 do dup 1 and if"
mycode "	rndmdoxors then 1 rshift rndm32 drop loop drop ; hidden"
mycode ": rndmgetnewseeds ( ~ 21100c ) rndmresetvars 8 0 do xishojmptbl"
mycode "	i 4* + @ rndmgopastbits loop s0 2@ s1 2@ s2 2@ s3 2@ ; hidden"
mycode ": rndmjump ( s0 s1 s2 s3 -- s0 s1 s2 s3 )"
mycode "	rndmgetseed 2>r 2>r 2>r 2>r rndmtoseed rndmgetnewseeds"
mycode "	2r> 2r> 2r> 2r> rndmtoseed ;"

@( -- user: 1block 1k )
@( 1block print 256 bytes vanaf addr in bytes )
@( 1k print 1k bytes in 4 blokken, input is in kilo -> 4 1k prints 4096 onwards )
mycode ": .noaschd drop 95 emit ; hidden"
mycode ": .aschd dup 31 > if dup 128 < if emit else"
mycode "	.noaschd then else .noaschd then ; hidden"
mycode ": .byteshd ( addr -- ) 16 0 do dup i + c@ .byte loop drop ; hidden"
mycode ": .ascshd ( addr -- ) 16 0 do dup i + c@ .aschd loop drop ; hidden"
mycode ": .blockhead cr 12 tab 16 0 do i .hex space loop ; hidden"
mycode ": .1linehd ( addr -- ) dup .word space dup .byteshd .ascshd ; hidden"
mycode ": 1block ( addr -- ) .blockhead cr 256 0 do dup i +"
mycode "    .1linehd cr 16 +loop drop ; hidden"
mycode ": 1kdump ( addr_in_k -- ) 1024 * 1024 0 do"
mycode "	dup i + 1block 256 +loop drop ;"
mycode ": dump ( addr u -- ) dup 0<> if .blockhead cr 1 8192 clip 0 do"
mycode "	dup i + .1linehd cr 16 +loop else drop then drop ;"

@( -- )
@ WAVE stuff
mycode ": wavecyclesused wvdiff @ 26 - ;"
mycode ": wavecyclesfree 34014 21 - wvdiff @ - ;"

@( -- )
@ SILLY stuff - 1 whole byte of scratch-storage 216c!!
mycode ": c!scratch [ hex 3F21505C decimal ] literal c! ;"
mycode ": c@scratch [ hex 3F21505C decimal ] literal c@ ;"

@ MISSING WORDS in wabiForth
mycode ": 2literal swap postpone literal postpone literal ; immediate"
@mycode ": umin 2dup u< if  drop exit then  swap drop ;"
@mycode ": umax 2dup u< if  swap  then  drop ;"

@( -- ) [IF] [ELSE] [THEN]
mycode ": refill -1 source-id = if false exit then true cr 0 >in !"
mycode "  ibaddr @ 1023 accept iblen ! true refill? ! ;"
mycode ": (* 0 begin begin begin drop cr refill 0= if exit then bl word count 2 ="
mycode "  until c@+ [char] * = until c@+ [char] ) = until drop ; immediate"
mycode ": $>upper bounds ?do i c@ >caps i c! loop ;"
mycode ": $>lower bounds ?do i c@ >lower i c! loop ;"
mycode ": string<> 2dup $>upper 2swap 2dup $>upper 2swap compare 0= ;"
mycode ": >fhere ( unprotected! ) trans tuck ! tuck count move ;"
mycode ": parea iblen @ >in @ ibaddr @ dup d+ ;"
mycode ": scan >r begin 2dup u> while dup c@ r@ <> while 1+ again"
mycode "  then then rdrop ;"
mycode ": skip ( cave tabs! ) >r begin 2dup u> while dup c@ r@ = while"
mycode "  1+ again then then rdrop ;"
mycode ": parse >r parea tuck r> scan tuck 1+ umin  ibaddr @ - >in ! over - ;"
mycode ": bl- begin parea bl skip dup ibaddr @ - >in ! ="
mycode "  while refill 0= until then ;"
mycode ": bl-word bl- bl parse >fhere ;"
mycode ": beyond >r begin parea r@ scan tuck = while drop refill 0="
mycode "  abort\" empty buffer\" repeat rdrop 1+ ibaddr @ - >in ! ;"
mycode ": .( [char] ) parse type ; IMMEDIATE"
mycode ": [ELSE] 1 begin begin begin ?dup 0= ?exit [char] [ beyond true >in +!"
mycode "  bl-word count 2dup $>upper s\" [THEN]\" 2over string<> 0= while 2drop 1-"
mycode "  repeat s\" [ELSE]\" 2over string<> 0= while 2drop dup 1 = +"
mycode "  repeat s\" [IF]\" string<> 0= - again ; immediate"
mycode ": [IF] 0<> ?exit postpone [else] ; immediate"
mycode ": [THEN] ; immediate"
mycode "0 value [if_variant"
mycode ": [IF false bl word count dup 0= abort\" no character after [IF\""
mycode "  bounds do i c@ >caps [if_variant >caps = if drop true leave then"
mycode "  loop postpone [IF] ; immediate"

@( -- user: CATCH THROW )
mycode "0 value excephandler"
mycode ": CATCH barrier [r14@] >r r13@ 4 + >r fp@ >r sp@ >r excephandler >r rp@"
mycode "  to excephandler barrier execute r> to excephandler rdrop rdrop"
mycode "  rdrop rdrop 0 barrier ;"
mycode ": THROW barrier dup 0= if drop exit then excephandler 0= if dup"
mycode "  -1 if abort then dup -2 if abort\" due to '-2 THROW' without a CATCH\""
mycode "  then then excephandler rp! r> to excephandler r> swap >r sp! drop"
mycode "  r> r> fp! r> r13! barrier r> gotoaddr ;"

@( -- user: all )
@( dit is een eerste test van het naar binnen halen van commandline dingen )
mycode ": .maccl 558 540 do i c@ emit loop ;" @ print mac-addres was:541
mycode ": .cmdlinecl cr 10000 613 do i c@ 0= if" @ was:614
mycode "	leave then i c@ emit loop ;" @ print de commmandline MYCMDLINE.txt

@( -- user: .weekday .monthname )
@( nice examples of string-arrays - here for names of days and mnths )
mycode "s\" sunmontuewedthufrisat\" sliteral daynames hidden"
mycode ": .weekday ( 1-7 -- ) 1- dup 0 7 within if"
mycode "	3 * daynames drop + 3 type else drop then ;"

mycode "s\" janfebmaraprmayjunjulaugsepoctnovdec\" 2constant monthnames hidden"
mycode ": .monthname ( 1-12 -- ) 1- dup 0 12 within if"
mycode "	3 * monthnames drop + 3 type else drop then ;"

@( I2C - see separate document for more comments )
mycode "haltinline"
mycode ": i2c_clear_all bit5 bsc1_c ! bit1 bit8 or bit9 or bsc1_s ! ;"
mycode ": i2c_init 4 2 setfuncgpio 4 3 setfuncgpio i2c_clear_all 1000 bsc1_div ! ;"
@( div factor is 1000 to get to 400 kHz )
mycode "i2c_init"
mycode ": i2c_stop bsc1_c @ bit15 invert and bsc1_c ! ;"
mycode ": i2c_address ( slave_address -- ) dup 8 120 within if bsc1_a ! else drop then ;"
mycode ": i2c_fifowrite ( byte -- ) 255 and bsc1_fifo ! ;"
mycode ": i2c_fiforead ( -- data ) bsc1_fifo @ ;"
mycode ": i2c_start ( -- ) ( start transfer ) bit7 bsc1_c ! ;"
mycode ": i2c_setdlen ( length data for transfer -- )"
mycode "	[hex] FFFF [decimal] and bsc1_dlen ! ;"
mycode ": i2c_getdlen ( -- no bytes left to handle ) bsc1_dlen @ ;"
mycode ": i2c_done ( -- true if done ) bsc1_s @ bit1 and formalflags ;"
mycode ": i2c_fifospace? ( -- true=>space for >= 1 byte )"
mycode "	bsc1_s @ bit4 and formalflags ;"
mycode ": i2c_fifodata? ( -- true -> fifo contains >= 1 byte )"
mycode "	bsc1_s @ bit5 and formalflags ;"
mycode ": i2c_err ( -- flag -> NACK received ) bsc1_s @ bit8 and formalflags ;"
mycode ": i2c_active ( -- true if active ) bsc1_s @ bit0 and formalflags ;"
mycode ": i2c_gets ( -- content of status reg ) bsc1_s @ 1023 and ;"
@ reset donebit added as test
mycode ": i2c_waitdone ( -- ) begin i2c_done until bit1 bsc1_s ! ; ( no time_out! )"

mycode ": i2c_startread bit15 bit7 or bit0 or bsc1_c ! ;"
mycode ": i2c_startwrite bit15 bit7 or bsc1_c ! ;"

@( -- i2cscan bsc1_show koers pitch roll )
mycode ": kl cr 6 tab ; : rij 18 tab ; hidden"
mycode ": bsc1_show"
mycode "	kl .\" address:\" bsc1_a @ rij .\" 0x\" .hex"
mycode "	kl .\" div:\" bsc1_div @ rij ."
mycode "	kl .\" data len:\" bsc1_dlen @ rij ."
mycode "	kl .\" control:\" bsc1_c @ rij .\" 0x\" .hex"
mycode "	kl .\" status:\" bsc1_s @ rij .\" 0x\" .hex"
mycode "	kl .\" - fifo emp:\" rij bsc1_s @ bit6 and .bit"
mycode "	kl .\" - NACK:\" rij bsc1_s @ bit8 and .bit"
mycode "	kl .\" - done:\" rij bsc1_s @ bit1 and .bit"
mycode "	kl .\" - CLKT:\" rij bsc1_s @ bit9 and .bit"
mycode "	kl .\" delay-reg:\" bsc1_del @ rij .\" 0x\" .hex"
mycode "	kl .\" CLKT tout:\" bsc1_clkt @ rij . ;"
@ as test commented out the 'reset done-bit'
mycode ": wbyte ( byte -- ) i2c_clear_all 1 i2c_setdlen i2c_fifowrite"
mycode "	i2c_startwrite i2c_waitdone ( bit1 bsc1_s ! ) ( reset done bit ) ;"
mycode ": w2byte ( byte1 byte2 -- ) i2c_clear_all 2 i2c_setdlen swap"
mycode "	i2c_fifowrite i2c_fifowrite i2c_startwrite i2c_waitdone ( bit1 bsc1_s ! ) ;"
mycode ": readbyte ( -- byte ) i2c_clear_all 1 i2c_setdlen i2c_startread"
mycode "	i2c_waitdone i2c_fiforead ( bit1 bsc1_s ! ) ( reset done bit ) ;"
mycode ": read16bit ( -- 16b ) i2c_clear_all 2 i2c_setdlen i2c_startread"
mycode "	i2c_waitdone i2c_fiforead 256 * i2c_fiforead + ( bit1 bsc1_s ! ) ;"

mycode ": koers ( -- koers*10 ) 96 i2c_address 2 wbyte read16bit ;"
mycode ": pitch ( -- pitch ) 96 i2c_address 4 wbyte readbyte ;"
mycode ": roll ( -- roll ) 96 i2c_address 5 wbyte readbyte ;"
mycode ": i2c_exist? ( address -- flag ) ( i2c_init ) i2c_clear_all bsc1_a !"
mycode "	0 i2c_setdlen i2c_startwrite i2c_waitdone i2c_err"
mycode "	if false else true then bit1 bsc1_s ! ( reset done bit ) ;"
mycode ": i2cheader cr 10 tab 16 0 do i .\" 0x\" .hex loop ; hidden"
mycode ": kl2 ( i -- ) cr 6 tab .\" 0x\" .hex ; hidden"
mycode ": docheck i2c_exist? if j 16 * i + .\" x\" .hex else .\" --- \" then ; hidden"
mycode ": 1stline 0 kl2 .\" g/s cba bus fut hsm hsm hsm hsm \" 16 8 do i docheck loop ; hidden"
mycode ": lstline 7 kl2 8 0 do i docheck loop .\" 10b 10b 10b 10b fut fut fut fut\" ; hidden"
mycode ": i2cscan i2cheader 1stline 7 1 do i kl2 16 0 do j 16 * i +"
mycode "    docheck loop loop lstline ;"

@( -- PortB on or off of a pin on the I2C bus 23007 )
mycode "variable portAout hidden 0 portAout !"
mycode "variable portBout hidden 0 portBout !"
mycode ": onportB ( 0-7 -- ) 7 and 1 swap lshift portBout @ or dup"
mycode "	portBout ! 19 swap w2byte ;" @ ( portBout = reg van portB )
mycode ": readlatchportB 21 wbyte readbyte ;" @ ( -- value latch )
mycode ": offportB ( 0-7 -- ) 7 and 1 swap lshift invert 255 and"
mycode "	portBout @ and dup portBout ! 19 swap w2byte ;"

@( -- rtc en aanverwant .rtc .datecamel .date .time rtcdumpram rtcsavepdregs rtcset rtcram! rtcram@ )
mycode "0 value rtcyear 0 value rtcmonth 0 value rtcday 0 value rtcweekday"
mycode "0 value rtchours 0 value rtcminutes 0 value rtcseconds"
mycode ": rtcstart 111 i2c_address 0 128 w2byte ( start clock ) ;"
mycode ": rtcstop 111 i2c_address 0 0 w2byte ( stop clock ) ;"
mycode ": getdigits ( bcd -- digital ) dup 4 rshift 10 * swap 15 and + ; hidden"
mycode ": makebcd ( digital -- bcd ) 10 /mod 4 lshift + ; hidden"
mycode ": rtcgetdata ( -- ) " @ ( puts rtc values into vars )
mycode "	111 i2c_address 0 wbyte 7 i2c_setdlen i2c_startread"
mycode "	i2c_waitdone 7 0 do i2c_fiforead loop"
mycode "	bit1 bsc1_s ! getdigits 2000 + to rtcyear"
mycode "	31 and getdigits to rtcmonth getdigits to rtcday"
mycode "	7 and getdigits to rtcweekday 63 and getdigits to rtchours"
mycode "	getdigits to rtcminutes 127 and getdigits to rtcseconds ;"

@( -- rtcRAM is 64 bytes with battery-backup )
mycode ": rtcheader cr 11 tab 16 0 do i .\" 0x\" .hex loop ; hidden"
mycode ": kl1 ( i -- ) cr 6 tab .\" 0x\" .hex ; hidden"
mycode ": rtcdumpram rtcheader 111 i2c_address 4 0 do i 2 + kl1 16 0"
mycode "	do j 16 * i + 32 + wbyte readbyte 4 .r loop loop ;"
mycode ": rtcclearram 111 i2c_address 96 32 do i 0 w2byte loop ;"
mycode ": rtcramrange? dup 32 96 within ; hidden"
mycode ": rtcram! ( byte rtcramaddress -- ) rtcramrange?"
mycode "	if swap 111 i2c_address w2byte else 2drop then ;"
mycode ": rtcram@ ( rtcramaddress -- byte ) rtcramrange?"
mycode "	if 111 i2c_address wbyte readbyte else drop -1 then ;"

mycode ": rtcsavepdregs ( -- ) ( saves to rtcram 32 - 39 )"
mycode "	111 i2c_address 3 wbyte readbyte bit4 and if ( save if pdbit set )"
mycode "	24 wbyte 8 i2c_setdlen i2c_startread i2c_waitdone"
mycode "	8 0 do i2c_fiforead loop bit1 bsc1_s ! ( reset done bit )"
mycode "	31 and getdigits 39 rtcram! ( mnd ) getdigits 38 rtcram! ( dag )"
mycode "	63 and getdigits 36 rtcram! ( hrs ) getdigits 37 rtcram! ( mins )"
mycode "	31 and getdigits 35 rtcram! ( mnd ) getdigits 34 rtcram! ( dag )"
mycode "	63 and getdigits 32 rtcram! ( hrs ) getdigits 33 rtcram! ( mins )"
mycode "	3 wbyte readbyte 3 swap w2byte ( clears pd-bit ) then ;"
mycode ": rtcset ( hh mm ss dd mm yy -- ) rtcstop 2 pick 2 pick 2 pick"
mycode "	dup makebcd 6 swap w2byte swap makebcd swap 2000 + leapyear?"
mycode "	true = if 32 or then 5 swap w2byte makebcd 4 swap w2byte"
mycode "	rtcsavepdregs 2000 + getweekday makebcd bit3 or 3 swap w2byte"
mycode "	( bit3 enables vbat backup ) -rot makebcd 1 swap w2byte ( mins to rtc )"
mycode "	makebcd 2 swap w2byte makebcd 128 or 0 swap w2byte ( secs and start clock ) ;"
mycode ": .date rtcgetdata rtcday .2d_ .\" -\" rtcmonth .2d_ .\" -\" rtcyear . ;"
mycode ": .datecamel rtcgetdata rtcday .2d_ rtcmonth .monthname rtcyear . ;"
mycode ": .time rtcgetdata rtchours .2d_ .\" :\" rtcminutes .2d_ "
mycode "	 .\" :\" rtcseconds .2d_ space ;"
mycode ": .rtc 6 tab .time rtcweekday .weekday space .datecamel ;"

@( ****************  banked EEPROM drivers **************** )
@( user: bankpage@, bankpage!, eeprom@, eeprom@next, block@, block! writelasttime??? )
@( #UPDATE: mcs must be updated to dmcs )
mycode ": page>i2c" @ ( page# -- memaddr busaddress )
mycode "	dup 3071 u> if drop 0 0 else dup 6 mod 80 +"
mycode "	swap 6 / 128 * swap then ; hidden"
mycode ": block>page ( block# -- page/-1 on error ) dup 0 384 within if 8* else drop -1 then ; hidden"
mycode ": addr>eeprom ( memaddr busaddress -- ) i2c_clear_all i2c_address"
mycode "	2 i2c_setdlen dup 8 rshift swap 255 and w2byte ; hidden"
mycode ": bankpage@ ( bankpage# *address -- ) >r i2c_clear_all page>i2c addr>eeprom"
mycode "	r> 128 i2c_setdlen i2c_startread 128 0 do 10000 0 do i2c_fifodata? if"
mycode "	dup i2c_fiforead swap c! 1+ leave then loop loop bit1 bsc1_s ! drop ;"
mycode "create writelasttime hidden 32 allot writelasttime 32 0 ( cfill ) fill"
mycode ": writelasttime??? 8 0 do writelasttime i 4* + @ cr u. loop ;"
mycode ": 5msmaxwait ( time -- ) 0 4999 clip 600 * 3000000 swap - 0 do loop ; hidden"
mycode ": eepromwritewait ( busaddr -- ) 80 - 4* writelasttime + @ mcs swap -"
mycode "	dup 5000 u< if 5msmaxwait else drop then ; hidden"
mycode ": waitclockupdate ( busaddr -- ) 80 - 0 max 7 min"
mycode "	cells writelasttime + mcs swap ! ; hidden"
mycode ": bankpage! ( *addres page# -- ) i2c_clear_all page>i2c dup dup >r i2c_address"
mycode "	eepromwritewait 130 i2c_setdlen dup 8 rshift i2c_fifowrite 255 and"
mycode "	i2c_fifowrite i2c_startwrite 128 0 do 10000 0 do i2c_fifospace? if"
mycode "	dup c@ i2c_fifowrite 1+ leave then loop loop bit1 bsc1_s !"
mycode "	drop r> waitclockupdate ;"
mycode ": setbankaddr ( addr 0-393215 -- ) dup 393215 u> if drop else 128 /mod"
mycode "	page>i2c -rot + swap addr>eeprom then ; hidden"
mycode ": eeprom@next ( -- byte ) readbyte ;"
mycode ": eeprom@ ( addr -- byte ) setbankaddr readbyte ;"
mycode ": block@ ( block# *addr-doel -- ) swap block>page dup -1 = if 2drop else"
mycode "	dup 8 + swap do dup i swap bankpage@ 128 + loop	drop then ;"
mycode ": block! ( *addr block# -- ) block>page dup -1 = if 2drop else"
mycode "	dup 8 + swap do dup i bankpage! 128 + loop drop then ;"

@( *******************  LCD drivers  *********************** )
@( user: lcdsleep, lcdoncor, lcdonnocur, lcdinit, lcdclear, lcdemit, lcddefchar, lcdxy, lcdhome )
@( user: lcdscreen, lcdtype, lcddefchar, lcdupdate, lcdupdatedic, lcdupdatecpu, lcdupdatemem )
mycode "39 constant lcdi2caddr hidden 8 constant bckltpin hidden"
mycode "variable backlite hidden bckltpin backlite !"
mycode "4 constant lcdenpin hidden 2 constant lcdrwpin hidden 1 constant lcdregpin hidden"
mycode ": bckltor backlite @ or ; hidden"
mycode ": lcdlowait 4000 waitmcs ; hidden"
mycode ": lcdshwait 60 waitmcs ; hidden"
mycode ": lcd! lcdi2caddr i2c_address 15 and 4 lshift bckltor dup"
mycode "	lcdenpin or swap w2byte 0 bckltor wbyte ;"
mycode ": lcd!data lcdi2caddr i2c_address 15 and 4 lshift lcdregpin"
mycode "	or bckltor dup lcdenpin or swap w2byte ;"
mycode ": lcdbkloff 0 backlite ! ;"
mycode ": lcdbklon 8 backlite ! ;"
mycode ": lcdclear 0 lcd! 1 lcd! lcdlowait ;"
mycode ": lcdsleep lcdbkloff 0 lcd! 8 lcd! lcdshwait ;"
mycode ": LcdOnNoCur lcdbklon 0 lcd! 12 lcd! lcdshwait ;"
mycode ": LcdOnCur lcdbklon 0 lcd! 15 lcd! lcdshwait ;"
mycode ": LcdEntryOn 0 lcd! 6 lcd! lcdshwait ; hidden"
mycode ": LcdSetMode 2 lcd! 8 lcd! lcdshwait ; hidden" @ only at initiation
mycode ": LcdEmit ( char -- ) 255 and dup 4 rshift 15 and lcd!data"
mycode "	15 and lcd!data ;"
mycode ": LcdType ( addr len -- ) 1 20 clip 0 do dup i + c@ lcdemit loop drop ;"
mycode ": LcdScrAddr ( pos -- ) 0 127 clip dup 4 rshift 15 and"
mycode "	8 or lcd! 15 and lcd! ;"
mycode ": LcdRamAddr ( pos -- ) 0 63 clip dup 4 rshift 7 and"
mycode "	4 or lcd! 15 and lcd! ;"
mycode ": LcdDefChar ( n, n, n, n, n, n, n, n, char# -- )"
mycode "	0 7 clip 8 * lcdramaddr 8 0 do 0 31 clip lcdemit loop ;	"
mycode ": LcdXy ( x y -- ) 0 3 clip dup 1 = if drop 64 then"
mycode "	dup 2 = if drop 20 then dup 3 = if drop 84 then"
mycode "	swap 0 19 clip + lcdscraddr ;"
mycode ": LcdHome 0 lcd! 2 lcd! lcdlowait ;"
mycode ": n>string s>d <# #s #> ; " @ ( n -- addr len ) ( unsigned - string ready for type )
mycode "s\" pi3b+ wabiForth4.2.0\" sliteral r1"
mycode "s\" cpu:      Mhz      C\" sliteral r2"
mycode "s\" def:                \" sliteral r3"
mycode "s\" ram:           free \" sliteral r4"
mycode ": defgraad 0 0 0 0 0 7 5 7 7 lcddefchar ( degree character ) ;"
mycode ": .LcdGraad 7 lcdemit ;"
mycode ": .Lcdr dup 4 < if 32 lcdemit then dup 3 < if 32 lcdemit then lcdtype ; hidden"
mycode ": .LcdMhz ( freq -- ) 1000000 / n>string .lcdr ; hidden"
mycode ": .LcdWords ( -- ) word# n>string .lcdr 32 lcdemit 32 lcdemit 32 lcdemit ; hidden"
mycode ": LcdScreen lcdclear 0 lcdscraddr r1 lcdtype"
mycode "	64 lcdscraddr r2 lcdtype 20 lcdscraddr r3 lcdtype"
mycode "	84 lcdscraddr r4 lcdtype 82 lcdscraddr .lcdgraad ; hidden"
mycode ": LcdUpdateFreq 69 lcdscraddr cpufreq .lcdmhz ;"
mycode ": LcdUpdateTemp	79 lcdscraddr cputemp n>string lcdtype ;"
mycode ": LcdUpdateDic 25 lcdscraddr .lcdwords ;"
mycode ": LcdUpdateMem 89 lcdscraddr memfree n>string lcdtype ;"
mycode ": LcdUpdate lcdupdatefreq lcdupdatedic lcdupdatemem lcdupdatetemp ;"
mycode ": LcdInit ( -- ) i2c_init 3 lcd! lcdlowait 3 lcd! lcdshwait lcdshwait"
mycode "	3 lcd! lcdshwait lcdshwait 2 lcd! lcdshwait lcdshwait"
mycode "	lcdsetmode lcdsleep lcdonnocur lcdentryon lcdclear ;"
mycode "  lcdi2caddr i2c_address lcdinit defgraad lcdscreen ( lcdupdate lcdsleep ) "
mycode "inlineasbefore"

@(  ***************  SIV info tables and stuff ******************  )
mycode " : sivinitsteps .\" core init steps: \" "
mycode "	sivc0init count . count . count . count . drop .\" (pass: >=18 13 12 0) \" cr ; hidden"
mycode "sivinitsteps"
mycode ": sivregregel ( addr2 addr1 step -- ) 12 tab dup ."
mycode "	swap over 4 * + 16 tab @ .32hex 4 * + 26 tab @ .32hex ; hidden"
mycode ": siv1314 cr .\" r13 entry step   core0     core1\" "
mycode "	10 0 do cr sivenc1r13s0 sivenc0r13s0 i sivregregel loop"
mycode "	cr cr .\" r14 entry step   core0     core1\" "
mycode "	10 0 do cr sivenc1r14s0 sivenc0r14s0 i sivregregel loop ;"

@(  *************************  PIXEL  **************************  )
mycode ": .pxc 10 0 do cr pxcounter @ . wait loop ;"
mycode ": .pxrefresh 10000000 pxrfrsrt @ / .1dec ;"

@(  *********************  START reports  **********************  )
mycode ": tstmmuc1 pxinitvars 4 + @ pxinitvars @ - 1- .\" core1 MMU-test: \" 3 spaces ."
mycode "	.\" mcs  (pass: < 700) \" cr ; hidden tstmmuc1"
mycode ": tstmmuc2 pxinitvars 12 + @ pxinitvars 8 + @ - 1- .\" core2 MMU-test: \" 3 spaces ."
mycode "	.\" mcs  (pass: < 700) \" cr ; hidden tstmmuc2"
mycode " .\" uptime: \" 7 spaces .uptime4boot space space .\" (pass: time =not 0s)\" cr "

@(  *************************  FLOATS  *************************  )
@(  user: F. REPRESENT
mycode "DECIMAL 7 CONSTANT maxdigits hidden 7 CONSTANT maxchars hidden"
mycode "CREATE buf hidden maxchars CHARS ALLOT"
mycode "maxchars CONSTANT PRECISION 2VARIABLE expsgn hidden"
mycode ": REPRESENT ( c-addr n1 -- n2 flag1 flag2 )"
mycode "	2DUP maxchars max 48 ( cfill ) fill maxdigits min 2>R"
mycode "	FDUP F0< 0 expsgn 2! fabs fdup f0= 0="
mycode "    IF BEGIN FDUP 1.0E F> WHILE 10.0E F/ 1 expsgn +! REPEAT"
mycode "    BEGIN FDUP 0.1E F< WHILE 10.0E F*  -1 expsgn +! REPEAT THEN"
mycode "	R@ 0 MAX 0 2dup <>	if DO 10.0E F* LOOP else 2drop then"
mycode "	FROUND FDUP 0 0 <# BEGIN 10.0E F/ FDUP FLOOR FSWAP FOVER F-"
mycode "    10.0E F* FROUND F>S 48 + HOLD FDUP F0= UNTIL FDROP #>"
mycode "	DUP R@ - expsgn +! 2R> ROT MIN 1 MAX CMOVE F0= IF 1 0"
mycode "    ELSE expsgn 2@ SWAP THEN TRUE ;"
mycode ": .mant  ( u -- ) buf OVER TYPE  [CHAR] . EMIT"
mycode "    buf PRECISION  ROT /STRING  TYPE ; hidden"
mycode ": f.nonan buf PRECISION REPRESENT IF IF [CHAR] - EMIT THEN"
mycode "	DUP >R 0 PRECISION 1+ WITHIN IF R> .mant ELSE"
mycode "    1 .mant [CHAR] E EMIT R> 1- . THEN ELSE"
mycode "    2drop buf maxchars -TRAILING TYPE SPACE THEN ; hidden"
mycode ": .neginf .\" -Inf\" ; : .posinf .\" +Inf\" ; : .nan .\" NaN\" ; hidden"
mycode ": infornan fdup> 9 lshift 9 rshift 0 = if fdup> 31 rshift 0 = if"
mycode "	.posinf else .neginf then else .nan then ; hidden"
mycode ": nonum? fdup> 1 lshift 24 rshift 255 = ; hidden"
mycode ": f. nonum? if infornan fdrop else f.nonan then ;"
mycode ": >rads f180/pi f/ ;" @ ( 18c )
mycode ": >degrees f180/pi f* ;"
mycode ": .ftop fdup> .32bin ;"
mycode ": flost fpsvars drop ; hidden : fpsreg fpsvars nip ; hidden"
mycode ": .addorfps ?dup 0= if .\" ftop-reg \" else .hex then ; hidden"
mycode ": .fsline rot kl .\" addr: \" .addorfps .\" pos: \" . .\" -> \" .hex ; hidden"
mycode ": .fsdb fdepth ?dup 0<> if 0 fdup> 0 .fsline dup 1 > if"
mycode "	1 do fpsreg i 4 * + dup @  i .fsline loop"
mycode "	else drop then else .\" fps-stack empty\" then ;"
mycode ": .fs fdepth ?dup 0<> if 0 do i fpick cr 8 spaces i . .\" -> \" f. loop"
mycode "	else .\" fps-stack empty\" then ;"

@(  ********************  PRINT FLAGS stuff  *******************  )
mycode " : tbr1 @ cr 8 tab ; : tbr2 28 tab 0= ;"
mycode " : .flags"
mycode " 	cr .\" ------- FUNCTION ---------- VALUE --------- \""
mycode "	maxinline tbr1 .\" inlining:\" dup tbr2 "
mycode "		if drop .\" off\" else .\" upto \" . .\" opcodes are inlined\" then"
mycode "	optimizedd tbr1 .\" pruning:\" tbr2 if .\" off\" else .\" active\" then"
mycode "	compoundflag tbr1 .\" compounding:\" tbr2 if .\" off\" else .\" active\" then"
mycode "	htb_flag tbr1 .\" hashtable:\" tbr2 if .\" not used\" else .\" in use\" then"
mycode "	rtuart tbr1 .\" UART receive:\" tbr2 if .\" interrupt-based\""
mycode "		else .\" real time - no interrupt\" then"
mycode "	nowarnflag tbr1 .\" duplicate warning:\" tbr2 if .\" on\" else .\" off\" then"
mycode "	cr .\" ------------------------------------------- \" ;"

@(  **********************  FIQ UART stuff  ********************  )
mycode ": uartrt true rtuart ! ;" @ tbd: add a clear buffer
mycode ": uartfiq false rtuart ! ;"
mycode ": uartclrrcvbuf 0 uartrwrite ! 0 uartrread ! ;" @ only safe when uartrt=true
mycode "uartclrrcvbuf uartfiq"
mycode ": fiqshow cr .\" uartrwrite: \" uartrwrite @ . "
mycode "	cr .\" uartrecread: \" uartrread @ . "
mycode "	cr .\" rtuart-flag: \" rtuart @ .flag ;"

@(  *********************  USER information  *******************  )
mycode ": knownissues"
mycode "	cr 3 spaces .\" 1)  SLITERAL not ANSI compatible  \""
mycode "	cr 3 spaces .\" 2)  SEE is a prototype - lots of opcodes missing \""
mycode "	cr 3 spaces .\" 3)  Confusingly, both system-VARIABLES and system-VALUES exist - take care \""
mycode "    cr 3 spaces .\" 4)  QUIT not ANSI compatible - consequences unknown \""
mycode "    cr 3 spaces .\" 5)  5CHDUMP does 1 more opcode than specified \" ;"

@(  *****************  END OF FORTH BASED SYS  *****************  )
mycode ": --ENDSYS-- cr .wabiversion cr ;"

@(  *********************  Assembler ARMv8  ******************** )
@ -- (C) J.J. Hoekstra - 2022
mycode ": assembler ;" @ allows forgetting the assembler
mycode ": BITFLD rot swap lshift or ; hidden : PUTCOND conbits 28 lshift or ; hidden"
mycode ": PUTIMM immbit 25 lshift or ; hidden : PUTIMMHW immbit 22 lshift or ; hidden"
mycode ": PUTOP2 or shftbits or	immbit if 8 bitfld then ; hidden"

mycode ": PUT3REGS putop2 16 bitfld 12 bitfld ; hidden"
mycode ": PUT2REGS putop2 12 bitfld ; hidden : PUTTREGS putop2 16 bitfld ; hidden"
mycode ": INIOPCDP putcond putimm ; hidden : I#, 1 to immbit ;"
mycode ": EOFOPC debug? if dup .\" opcode,: \" .hex then opcode, opcodereset ; hidden"
mycode ": SHFT# create , does> @ 4 lshift swap 7 lshift or to shftbits ; hidden"
mycode ": SHFTR create , does> @ 4 lshift swap 8 lshift or to shftbits ; hidden"
mycode "  0 shft# lsl#,	 2 shft# lsr#, 	 4 shft# asr#,	 6 shft# ror#,"
mycode "  1 shftr lslr,	 3 shftr lsrr,	 5 shftr asrr,	 7 shftr rorr,"

mycode ": (REG create , does> @ ; hidden 0 (reg r0, 0 (reg p14, 0 (reg c0, 1 (reg r1,"
mycode "  1 (reg p15, 1 (reg c1, 2 (reg r2, 2 (reg c2, 3 (reg r3, 3 (reg c3, 4 (reg r4,"
mycode "  4 (reg c4, 5 (reg r5, 5 (reg c5, 5 (reg top, 6 (reg r6, 6 (reg c6, 6 (reg fps,"
mycode "  7 (reg r7, 7 (reg c7, 7 (reg i, 8 (reg r8, 8 (reg c8, 8 (reg lim, 9 (reg r9,"
mycode "  9 (reg c9, 9 (reg dts, 10 (reg r10, 10 (reg c10, 10 (reg uss, 11 (reg r11,"
mycode "  11 (reg c11, 11 (reg v, 12 (reg r12, 12 (reg c12, 12 (reg w, 13 (reg r13,"
mycode "  13 (reg c13, 13 (reg sp, 14 (reg r14, 14 (reg c14, 14 (reg lr, 15 (reg r15,"
mycode "  15 (reg c15, 15 (reg pc, : (COND create , does> @ to conbits ; hidden"
mycode "  0 (cond eq, 1 (cond ne, 2 (cond cs, 2 (cond hs, 3 (cond cc, 3 (cond lo,"
mycode "  4 (cond mi, 5 (cond pl, 6 (cond vs, 7 (cond vc, 8 (cond hi, 9 (cond ls,"
mycode "  10 (cond ge, 11 (cond lt, 12 (cond gt, 13 (cond le,	14 (cond al,"

mycode ": (3ROP create , does> @ iniopcdp put3regs eofopc ; hidden"
mycode ": (2ROP create , does> @ iniopcdp put2regs eofopc ; hidden"
mycode ": (TSOP create , does> @ iniopcdp puttregs eofopc ; hidden hex"
mycode "  0000000 (3rop and, 0200000 (3rop eor, 0400000 (3rop sub, 0600000 (3rop rsb,"
mycode "  0800000 (3rop add, 0A00000 (3rop adc, 0C00000 (3rop sbc, 0E00000 (3rop rsc,"
mycode "  1100000 (tsop tst, 1300000 (tsop teq,	1500000 (tsop cmp, 1700000 (tsop cmn,"
mycode "  1800000 (3rop orr, 1A00000 (2rop mov,	1C00000 (3rop bic, 1E00000 (2rop mvn,"
mycode "  0100000 (3rop ands, 0300000 (3rop eors, 0500000 (3rop subs, 0700000 (3rop rsbs,"
mycode "  0900000 (3rop adds, 0B00000 (3rop adcs, 0D00000 (3rop sbcs, 0F00000 (3rop rscs,"
mycode "  1900000 (3rop orrs, 1B00000 (2rop movs, 1D00000 (3rop bics, 1F00000 (2rop mvns,"
mycode "  6A00010 (3rop ssat, 6E00010 (3rop usat, 6800010 (3rop pkhbt,"
mycode "  6800050 (3rop pkhtb, decimal"

mycode ": PUT3MULREGS 8 bitfld 0 bitfld 16 bitfld ; hidden"
mycode ": PUT4REGS 12 bitfld put3mulregs ; hidden"
mycode ": PUT4REGSD put3mulregs 12 bitfld ; hidden"
mycode ": (MULT create , does> @ putcond put3mulregs eofopc ; hidden"
mycode ": (MLAT create , does> @ putcond put4regs eofopc ; hidden"
mycode ": (MLLT ( n -- ) create , does> @ putcond put4regsd eofopc ; hidden hex"
mycode "  00000090 (mult mul,     00100090 (mult muls,    00200090 (mlat mla,"
mycode "  00300090 (mlat mlas,    00800090 (mllt smull,   00900090 (mllt smulls,"
mycode "  00E00090 (mllt smlal,	  00F00090 (mllt smlals,  00C00090 (mlat umull,"
mycode "  00D00090 (mlat umulls,  00A00090 (mllt umlal,   00B00090 (mllt umlals,"
mycode "  00400090 (mllt umaal,	  07800010 (mlat usada8,  00600090 (mlat mls,"
mycode "  01000080 (mlat smlabb,  010000C0 (mlat smlabt,  010000A0 (mlat smlatb,"
mycode "  010000E0 (mlat smlatt,  07000010 (mlat smlad,	  07000030 (mlat smladx,"
mycode "  01400080 (mllt smlalbb, 014000C0 (mllt smlalbt, 014000A0 (mllt smlaltb,"
mycode "  014000E0 (mllt smlaltt, 07400010 (mllt smlald,  07400030 (mllt smlaldx,"
mycode "  01200080 (mlat smlawb,  012000C0 (mlat smlawt,  07000050 (mlat smlsd,"
mycode "  07000070 (mlat smlsdx,  07500010 (mlat smmla,   07500030 (mlat smmlar,"
mycode "  075000D0 (mlat smmls,   075000F0 (mlat smmlsr,  0750F010 (mult smmul,"
mycode "  0750F030 (mult smmulr,  0700F010 (mult smuad,   0700F030 (mult smuadx,"
mycode "  01600080 (mult smulbb,  016000C0 (mult smulbt,  016000A0 (mult smultb,"
mycode "  016000E0 (mult smultt,  012000A0 (mult smulwb,  012000E0 (mult smulwt,"
mycode "  0700F050 (mult smusd,   0700F070 (mult smusdx,  07400050 (mllt smlsld,"
mycode "  07400070 (mllt smlsldx, decimal"

mycode ": PUTREGSLS shftbits or offbits or indbits or"
mycode "  regbit if 0 bitfld then 16 bitfld 12 bitfld ; hidden"
mycode ": (ROFF create , does> @ to indbits 1 to regbit 0 to immbit ; hidden"
mycode ": (IOFF create , does> @ to indbits 0 to regbit 1 to immbit to offbits ; hidden"
mycode "  hex 3800000 (roff +], 3000000 (roff -], 3A00000 (roff +]!, 3200000 (roff -]!,"
mycode "  2A00000 (roff ]+!, 2200000 (roff ]-!, 1800000 (ioff i+], 1000000 (ioff i-],"
mycode "  1A00000 (ioff i+]!, 1200000 (ioff i-]!, 0A00000 (ioff ]i+!, 0200000 (ioff ]i-!,"
mycode ": (LOST create , does> @ putcond putregsls eofopc ; hidden hex"
mycode "  4100000 (lost ldr, 4000000 (lost str, 4500000 (lost ldrb,"
mycode "  4400000 (lost strb, 4300000 (lost ldrt, 4200000 (lost strt,"
mycode "  4700000 (lost ldrbt, 4600000 (lost strbt, decimal"

mycode ": splitnib dup >r 4 rshift 8 lshift r> 15 and or ; hidden"
mycode ": putregshw offbits splitnib or	putimmhw indbits or"
mycode "  regbit if 0 bitfld then 16 bitfld 12 bitfld ; hidden"
mycode ": FDDLB25 1 25 lshift invert and ; hidden"
mycode ": (LOSTHW create , does> @ putcond putregshw fddlb25 eofopc ; hidden hex"
mycode "  001000B0 (losthw ldrh,  000000B0 (losthw strh,  001000F0 (losthw ldrsh,"
mycode "  001000D0 (losthw ldrsb, 000000D0 (losthw ldrd,  000000F0 (losthw strd,"
mycode "  007000B0 (losthw ldrht, 006000B0 (losthw strht, 007000F0 (losthw ldrsht,"
mycode "  007000D0 (losthw ldrsbt, decimal"

mycode ": put2regsla 16 bitfld 12 bitfld ; hidden"
mycode ": (LA2 create , does> @ putcond put2regsla eofopc ; hidden"
mycode "  hex 01900C9F (la2 lda, 01D00C9F (la2 ldab, 01F00C9F (la2 ldah,"
mycode "  01900E9F (la2 ldaex,  01D00E9F (la2 ldaexb, 01F00E9F (la2 ldaexh,"
mycode "  01B00E9F (la2 ldaexd, 01900F9F (la2 ldrex,  01B00F9F (la2 ldrexd,"
mycode "  01D00F9F (la2 ldrexb, 01F00F9F (la2 ldrexh, decimal"

mycode ": put2regssr 16 bitfld 0 bitfld ; hidden"
mycode ": (SL2 create , does> @ putcond put2regssr eofopc ; hidden"
mycode "  hex 0180FC90 (sl2 stl, 01C0FC90 (sl2 stlb, 01E0FC90 (sl2 stlh, decimal"

mycode ": put3regscon 16 bitfld 0 bitfld 12 bitfld ; hidden"
mycode ": (SX3 create , does> @ putcond put3regscon eofopc ; hidden hex"
mycode "  01800E90 (sx3 stlex,  01C00E90 (sx3 stlexb, 01E00E90 (sx3 stlexh,"
mycode "  01A00E90 (sx3 stlexd, 01000050 (sx3 qadd,   01400050 (sx3 qdadd,"
mycode "  01600050 (sx3 qdsub,	01200050 (sx3 qsub,   01800F90 (sx3 strex,"
mycode "  01C00F90 (sx3 strexb, 01A00F90 (sx3 strexd, 01E00F90 (sx3 strexh, decimal"

mycode ": put3regsconrev 0 bitfld 16 bitfld 12 bitfld ; hidden"
mycode ": (3rreg create , does> @ putcond put3regsconrev eofopc ; hidden hex"
mycode "  06100F10 (3rreg sadd16,  06100F30 (3rreg sasx,   06100F50 (3rreg ssax,"
mycode "  06200F10 (3rreg qadd16,  06200F30 (3rreg qasx,   06200F50 (3rreg qsax,"
mycode "  06300F10 (3rreg shadd16, 06300F30 (3rreg shasx,  06300F50 (3rreg shsax,"
mycode "  06500F10 (3rreg uadd16,  06500F30 (3rreg uasx,   06500F50 (3rreg usax,"
mycode "  06600F10 (3rreg uqadd16, 06600F30 (3rreg uqasx,  06600F50 (3rreg uqsax,"
mycode "  06700F10 (3rreg uhadd16, 06700F30 (3rreg uhasx,  06700F50 (3rreg uhsax,"
mycode "  06100F70 (3rreg ssub16,  06100F90 (3rreg sadd8,  06100FF0 (3rreg ssub8,"
mycode "  06200F70 (3rreg qsub16,  06200F90 (3rreg qadd8,  06200FF0 (3rreg qsub8,"
mycode "  06300F70 (3rreg shsub16, 06300F90 (3rreg shadd8, 06300FF0 (3rreg shsub8,"
mycode "  06500F70 (3rreg usub16,  06500F90 (3rreg uadd8,  06500FF0 (3rreg usub8,"
mycode "  06600F70 (3rreg uqsub16, 06600F90 (3rreg uqadd8, 06600FF0 (3rreg uqsub8,"
mycode "  06700F70 (3rreg uhsub16, 06700F90 (3rreg uhadd8, 06700FF0 (3rreg uhsub8,"
mycode "  decimal hex 10011001 decimal constant regmarker hidden"
mycode ": putregsbt indbits or regbits or 16 bitfld ; hidden"
mycode ": SETBIT dup 15 > if .\" register expected -> \" abort then"
mycode "  1 swap lshift regbits or to regbits ; hidden"
mycode ": BLREGS 16 0 do dup regmarker = if leave then setbit loop drop ;	hidden"
mycode ": r-r, 2dup > if swap then 2dup = if drop setbit else 1+ swap do i setbit loop"
mycode "  then ; : BL} ( n -- ) create , does> @ to indbits blregs ; hidden"
mycode "  hex 00000000 bl} }, 00200000 bl} }!, 00400000 bl} }^, 00600000 bl} }!^,"
mycode "  decimal : {, regmarker 0 to regbits ;"
mycode ": (BLTR ( n -- ) create 20 lshift , does> @ putcond putregsbt eofopc ; hidden"
mycode "  hex	99 (bltr ldmed,	99 (bltr ldmib,	89 (bltr ldmfd,	89 (bltr ldmia,"
mycode "  91 (bltr ldmea,	91 (bltr ldmdb,	81 (bltr ldmfa,	81 (bltr ldmda,"
mycode "  98 (bltr stmfa,	98 (bltr stmib,	88 (bltr stmea,	88 (bltr stmia,"
mycode "  90 (bltr stmfd,	90 (bltr stmdb,	80 (bltr stmed,	80 (bltr stmda, decimal"

mycode ": putregsmmcc 5 bitfld 0 bitfld	16 bitfld 12 bitfld	21 bitfld 8 bitfld ; hidden"
mycode ": putregsmmccrr 0 bitfld 16 bitfld 12 bitfld 4 bitfld 8 bitfld ; hidden"
mycode ": (MMCC create , does> @ putcond putregsmmcc eofopc ; hidden"
mycode ": (MMRR create , does> @ putcond putregsmmccrr eofopc ; hidden hex"
mycode "  0E000E10 (MMCC mcr, 0C400E00 (MMRR mcrr, 0E100E10 (MMCC mrc, 0C500E00 (MMRR mrrc,"
mycode ": mrs, 01000000 putcond shftbits or C bitfld eofopc ;"
mycode ": msr, 0120F20 putcond shftbits or 0 bitfld eofopc ;"
mycode ": (SRB create , does> @ to shftbits ; hidden"
mycode "  hex 4F0000 (srb spsr, 0F0000 (srb cpsr, 0F0000 (srb apsr, 000200 (srb r8_usr,"
mycode "  010200 (srb r9_usr, 020200 (srb r10_usr, 030200 (srb r11_usr, 040200 (srb r12_usr,"
mycode "  050200 (srb sp_usr, 060200 (srb lr_usr, 080200 (srb r8_fiq, 090200 (srb r9_fiq,"
mycode "  0A0200 (srb r10_fiq, 0B0200 (srb r11_fiq, 0C0200 (srb r12_fiq, 0D0200 (srb sp_fiq,"
mycode "  0E0200 (srb lr_fiq, 000300 (srb lr_irq, 010300 (srb sp_irq, 020300 (srb lr_svc,"
mycode "  030300 (srb sp_svc, 040300 (srb lr_abt, 050300 (srb sp_abt, 060300 (srb lr_und,"
mycode "  070300 (srb sp_und, 0C0300 (srb lr_mon, 0D0300 (srb sp_mon, 0E0300 (srb elr_hyp,"
mycode "  0F0300 (srb sp_hyp, 4E0200 (srb spsr_fiq, 400300 (srb spsr_irq,"
mycode "  420300 (srb spsr_svc, 440300 (srb spsr_abt, 460300 (srb spsr_und,"
mycode "  4C0300 (srb spsr_mon, 4E0300 (srb spsr_hyp, decimal"

mycode ": put3regsmm 8 bitfld 0 bitfld 16 bitfld ; hidden"
mycode ": (MM3 create , does> @ putcond put3regsmm eofopc ; hidden"
mycode "  hex 0710F010 (mm3 sdiv, 0730F010 (mm3 udiv, E1400040 (mm3 crc32w,"
mycode "  E1400040 (mm3 crc32, E1200040 (mm3 crc32h, E1000040 (mm3 crc32b,"
mycode "  E1400240 (mm3 crc32cw, E1400240 (mm3 crc32c, E1200240 (mm3 crc32ch,"
mycode "  0780F010 (mm3 usad8, E1000240 (mm3 crc32cb, decimal"
mycode ": bfc, [hex] 07C00010 [decimal] putcond swap 1- over + swap"
mycode "  16 bitfld 7 bitfld 15 0 bitfld 12 bitfld eofopc ;"
mycode ": putregsbf swap 1- ( over + ) swap 16 bitfld 7 bitfld 0 bitfld 12 bitfld ; hidden"
mycode ": (BF create , does> @ putcond putregsbf eofopc ; hidden"
mycode "  hex 07C00010 (bf bfi, 07A00050 (bf sbfx, 07E00050 (bf ubfx,"

mycode ": putregsxt shftbits or 0 bitfld 12 bitfld ; hidden"
mycode ": put3regsxta shftbits or 0 bitfld 16 bitfld 12 bitfld ; hidden"
mycode ": (XT create , does> @ putcond putregsxt eofopc ; hidden"
mycode ": (XTA create , does> @ putcond put3regsxta eofopc ; hidden"
mycode "  hex 06AF0070 (xt sxtb, 068F0070 (xt sxtb16, 06BF0070 (xt sxth,"
mycode "  06EF0070 (xt uxtb, 06CF0070 (xt uxtb16, 06FF0070 (xt uxth,"
mycode "  06A00070 (xta sxtab, 06800070 (xta sxtab16, 06B00070 (xta sxtah,"
mycode "  06E00070 (xta uxtab, 06C00070 (xta uxtab16, 06F00070 (xta uxtah,"
mycode "  06800FB0 (xta sel, 06A00F30 (xta ssat16, 06E00F30 (xta usat16, decimal"

mycode ": put2regrev 0 bitfld 12 bitfld ; hidden"
mycode ": (RV create , does> @ putcond put2regrev eofopc ; hidden hex"
mycode "  06BF0F30 (rv rev, 06BF0FB0 (rv rev16, 06FF0F30 (rv rbit, 06FF0FB0 (rv revsh,"
mycode "  016F0F10 (rv clz, decimal"

mycode ": (ZRC create , does> @ putcond eofopc ; hidden"
mycode "  hex 0320F004 (zrc sev, 0320D005 (zrc sevl, 0320F002 (zrc wfe, 0320F003 (zrc wfi,"
mycode "  0320F001 (zrc yield, 0320F000 (zrc nop, 0160006E (zrc eret,"
mycode ": (ZR create , does> @ eofopc ; hidden hex"
mycode "  E320F014 (zr csdb, F57FF044 (zr pssbb, F57FF01F (zr clrex,"
mycode "  F1010000 (zr setendle F1010200 (zr setendbe decimal"

mycode ": putoption shftbits 0= if 15 to shftbits then shftbits or ; hidden"
mycode ": (ZR1O create , does> @ putoption eofopc ; hidden"
mycode "  hex F57FF050 (zr1o dmb, F57FF040 (zr1o dsb, F57FF060 (zr1o isb, decimal"
mycode ": (OPB create , does> @ to shftbits ; hidden hex"
mycode "  F (opb sy, E (opb st, D (opb ld, B (opb ish, A (opb ishst, 9 (opb ishld,"
mycode "  7 (opb nsh, 6 (opb nshst, 5 (opb nshld, 3 (opb osh, 2 (opb oshst, 1 (opb oshld, decimal"

mycode ": (1rn create , does> @ 16 bitfld eofopc ; hidden hex"
mycode "  F8900A00 (1rn rfe, F8B00A00 (1rn rfe!, F8900A00 (1rn rfeia, F8B00A00 (1rn rfeia!,"
mycode "  F8100A00 (1rn rfeda, F8300A00 (1rn rfeda!, F9100A00 (1rn rfedb, F9300A00 (1rn rfedb!,"
mycode "  F9900A00 (1rn rfeib, F9B00A00 (1rn rfeib!, decimal"

mycode ": put1reg8imm immbit if offbits or indbits else bit23 or then or 16 bitfld ; hidden"
mycode ": (LSXCE create , does> @ putcond put1reg8imm eofopc ; hidden"
mycode "  hex 0C105E00 (lsxce ldc,	0C005E00 (lsxce stc, decimal"

mycode ": setu12imm dup 0< if negate else bit23 or then ; hidden"
mycode ": put1reg12imm swap setu12imm swap 0 bitfld 16 bitfld ; hidden"
mycode ": (1r12 create , does> @ put1reg12imm eofopc ; hidden"
mycode "  hex F510F000 (1r12 pldw, F550F000 (1r12 pld, F450F000 (1r12 pli,"

mycode ": put1reg16imm swap dup >r FFF and swap 0 bitfld r> F000 and C rshift swap"
mycode "  10 bitfld C bitfld ; hidden"
mycode ": (0R16 create , does> @ putcond put1reg16imm eofopc ; hidden"
mycode "  03400000 (0r16 movt, 03000000 (0r16 movw, decimal"

mycode "hex : bkpt, FFFF and splitnib E1200070 or eofopc ; decimal"
mycode "hex : hvc,  FFFF and splitnib E1400070 or eofopc ; decimal"
mycode "hex : smc, 01600070 putcond swap F and or eofopc ; decimal"
mycode "hex : svc, 0F000000 putcond swap FFFFFF and or eofopc ; decimal"

mycode ": putregscps shftbits or offbits or ; hidden"
mycode ": (cps create , does> @ putregscps eofopc ; hidden"
mycode "  hex F1000010 (cps cps, F10C0000 (cps cpsid,	F1080000 (cps cpsie,"
mycode ": (mod create , does> @ to offbits ; hidden 20000 (mod user, 20001 (mod fiq,"
mycode "  20002 (mod irq, 20003 (mod supervisor, 20006 (mod monitor, 20007 (mod abort,"
mycode "  2000A (mod hyp, 2000B (mod undefined, 2000F (mod system,"
mycode ": (fl create , does> @ to shftbits ; hidden 100 (fl <a>, 180 (fl <ai>,"
mycode "  140 (fl <af>, 1C0 (fl <aif>, 080 (fl <i>, 0C0 (fl <if>, 040 (fl <f>, decimal"
mycode ": bx, [hex] 012FFF10 [decimal] putcond 0 bitfld eofopc ;"
mycode ": blx, [hex] 012FFF30 [decimal] putcond 0 bitfld eofopc ;"

mycode ": (0rmod create , does> @ offbits or bit17 bitclear ( eofopc ) .hex ; hidden"
mycode "hex	F8CD0500 (0rmod srs,	F8ED0500 (0rmod srs!,	F8CD0500 (0rmod srsia,"
mycode "	F8ED0500 (0rmod srsia!,	F84D0500 (0rmod srsda,	F86D0500 (0rmod srsda!,"
mycode "	F94D0500 (0rmod srsdb,	F96D0500 (0rmod srsdb!, F9CD0500 (0rmod srsib,"
mycode "	F9ED0500 (0rmod srsib!, decimal"

mycode "  create LBLTBL 16 cells allot hidden : CLRLBLTBL lbltbl 16 0 fastfill ; hidden"
mycode ": LBL@ ( lbl# -- addr ) cells lbltbl + @ ; hidden"
mycode ": LBL! ( n lbl# -- ) cells lbltbl + ! ; hidden"
mycode ": LBL[ clrlbltbl ; immediate : BOFFSET 8 + - 6 lshift 8 rshift ; hidden"
mycode "  hex : CALCADDR FFFFFF and 8 lshift 8 arshift dup 0<> if cells + else nip then ;"
mycode "  hidden 0 value ORIGVAL hidden"
mycode ": RESOLVE) begin >r	r@ @ 1 bitclear to origval here r@ boffset origval"
mycode "  FF000000 and or r@ safe! r> origval calcaddr dup 0= until drop ; hidden decimal"
mycode ": RESOLVE lbl@ 	dup 1 and if 1 bitclear resolve) else drop then ; hidden"
mycode ": HERE2LBL cells lbltbl + here swap ! ; hidden"
mycode ": LB# create , does> @ dup resolve here2lbl ; hidden"
mycode "  00 lb# label0: 01 lb# label1: 02 lb# label2: 03 lb# label3: 04 lb# label4:"
mycode "  05 lb# label5: 06 lb# label6: 07 lb# label7: 08 lb# label8: 09 lb# label9:"
mycode "  10 lb# label10: 11 lb# label11: 12 lb# label12: 13 lb# label13:"
mycode "  14 lb# label14: 15 lb# label15: : 1STLBLLNK here 1 or swap lbl! ; hidden"
mycode ": NXTLBLLNK here - 6 lshift 8 rshift swap here 1 or swap lbl! ; hidden"
mycode ": LBLADDR dup lbl@ dup 0= if drop 1stlbllnk 0 else dup 1 and if	1 bitclear"
mycode "  nxtlbllnk else nip here boffset then then ; hidden"
mycode ": (LB# create , does> @ lbladdr ; hidden hex 00 (lb# >lb0, 01 (lb# >lb1,"
mycode "  02 (lb# >lb2, 03 (lb# >lb3, 04 (lb# >lb4, 05 (lb# >lb5, 06 (lb# >lb6,"
mycode "  07 (lb# >lb7, 08 (lb# >lb8, 09 (lb# >lb9, 10 (lb# >lb10, 11 (lb# >lb11,"
mycode "  12 (lb# >lb12, 13 (lb# >lb13, 14 (lb# >lb14, 15 (lb# >lb15,"
mycode "  decimal : (BRH create 24 lshift , does> @ or eofopc ; hidden hex"
mycode "  0A (brh beq, 1A (brh bne, 2A (brh bcs, 3A (brh bcc, 4A (brh bmi, 5A (brh bpl,"
mycode "  6A (brh	bvs, 7A (brh bvc, 8A (brh bhi, 9A (brh bls,	AA (brh	bge, BA (brh blt,"
mycode "  CA (brh	bgt, DA (brh ble, EA (brh b, EA (brh bal, 2A (brh bhs, 3A (brh blo,"
mycode "  0B (brh bleq,	1B (brh blne, 2B (brh blcs, 3B (brh blcc, 4B (brh blmi,"
mycode "  5B (brh blpl,	6B (brh	blvs, 7B (brh blvc, 8B (brh blhi, 9B (brh blls,"
mycode "  AB (brh blge,	BB (brh bllt, CB (brh blgt, DB (brh blle, EB (brh bl,"
mycode "  EB (brh blal, 2B (brh blhs, 3B (brh bllo, decimal"
mycode ": --end_of_assembler-- .\" ARMv8 assembler -- (c) 2022 - J.J. Hoekstra\" ;"

@( -- user: CATCH THROW )
@mycode "0 value excephandler"
@mycode ": CATCH [r14@] >r r13@ 4 + >r fp@ >r sp@ >r excephandler >r rp@"
@mycode "  to excephandler [ dsb, isb, ] execute r> to excephandler rdrop rdrop"
@mycode "  rdrop rdrop 0 [ dsb, isb, ] ;"
@mycode ": THROW [ dsb, isb, ] dup 0= if drop exit then excephandler 0= if dup"
@mycode "  -1 if abort then dup -2 if abort\" due to '-2 THROW' without a CATCH\""
@mycode "  then then excephandler rp! r> to excephandler r> swap >r sp! drop"
@mycode "  r> r> fp! r> r13! [ dsb, isb, ] r> gotoaddr ;"

@(  *****************  UPDATE LCD and HASHTABLE  *****************  )
mycode "lcdupdate lcdsleep usehtb ( htb_redo )"

