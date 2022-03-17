﻿#load "MultiSet.fs"

open MultiSet


let emptyMS = empty

let with1 = addSingle 1 emptyMS

let with2 = addSingle 1 with1

remove 1 3u (add 1 5u emptyMS)

//foldBack (fun e x acc -> acc + (e * (int x))) (add 7 3u (add 5 6u (add 9 9u empty))) 0

//toList (add 7 3u (add 5 6u (add 9 9u (add 27 2u empty))))

//numItems true (ofList [for i in 1..100 do for j in i..100 do yield i] |> map (fun i -> i % 3 = 0))

let s5 = ofList [for i in 1..100 do for j in i..100 do yield i]
numItems 5 (union s5 s5 |> map (fun i -> i % 10))

let s4 = ofList [for i in 1..100 do for j in i..100 do yield i]
size (union s4 s4 |> map (fun i -> i % 10))


//let s35 = ofList [for i in 1..100 do for j in i..100 do yield i]
//let s35' = ofList [for i in 1..100 do for j in i..100 do yield i; yield i]
//numItems 5 (sum s35 s35' |> map (fun i -> i % 10))

//let s22 = ofList [for i in 1..100 do for j in i..100 do yield i]
//isEmpty (subtract s22 s22)
//size (add "Hello" 4u empty)


//[for i in 1..100 do for j in i..100 do yield i].Length
//size s11

//numItems 90 (ofList [for i in 1..100 do for j in i..100 do yield i])
//numItems 101 (ofList [for i in 1..100 do for j in i..100 do yield i])

let s11 = ofList [for i in 1..100 do for j in i..100 do yield i]
size (intersection s11 s11)

let s13 = ofList [for i in 1..100 do for j in i..100 do yield i]
let s14 = ofList [for i in 1..100 do for j in i*2..100 do yield i]
size (intersection s13 s14) // should be 2500u

intersection s14 s13 |> fun (MS s) -> s.[1]

remove 1 8u (ofList [1;1;1;1;1;1;1])

100u - 99u

let p = ofList [0;0;1;1;2;2;3]
let q = ofList [0;0;1;2;4]
toList p
numItems 0 p


toList (intersection p q)

add 0 0u emptyMS




#load "Dictionary.fs"
open Dictionary
let emptyDict = empty ()
insert "hello" emptyDict
insert "hel" (insert "hello" emptyDict)
insert "hoo" (insert "hello" emptyDict)

lookup "hi" (insert "ho" (empty ()))

"".Length

Dictionary.empty () |> Dictionary.insert "HELLO" |> Dictionary.insert "HE"
Dictionary.lookup "HE" (Dictionary.empty () |> Dictionary.insert "HELLO" |> Dictionary.insert "HE")
Dictionary.lookup "HE" (Dictionary.empty () |> Dictionary.insert "HE" |> Dictionary.insert "HELLO")
Dictionary.lookup "HE" (Dictionary.empty () |> Dictionary.insert "HE" |> Dictionary.insert "HELLO" |> Dictionary.insert "ABC")

Dictionary.lookup "TONIGHT" (Dictionary.empty () |> Dictionary.insert "TONIGHT" |> Dictionary.insert "TIME" |> Dictionary.insert "FLOAT" |> Dictionary.insert "THINGS" |> Dictionary.insert "TON" |> Dictionary.insert "TON")

Dictionary.lookup "DEKED" (Dictionary.empty () |> Dictionary.insert "DAD" |> Dictionary.insert "DEED" |> Dictionary.insert "DEIFLIED" |> Dictionary.insert "DEKED" |> Dictionary.insert "DEWED" |> Dictionary.insert "DID")

Dictionary.lookup "DEED" (Dictionary.empty () |> Dictionary.insert "DM" |> Dictionary.insert "DEED")


let temp = System.Collections.Generic.Dictionary ()
temp["hi"] <- 1

temp

(*
AA
AAH
AAHED
AAHING
AAHS
AAL
AALII
AALIIS
AALS
AARDVARK
AARDVARKS
AARDWOLF
AARDWOLVES
AARGH
AARRGH
AARRGHH
AAS
AASVOGEL
AASVOGELS
AB
ABA
ABACA
ABACAS
ABACI
ABACK
ABACTERIAL
ABACUS
ABACUSES
ABAFT
ABAKA
ABAKAS
ABALONE
ABALONES
ABAMP
ABAMPERE
ABAMPERES
ABAMPS
ABANDON
ABANDONED
ABANDONER
ABANDONERS
ABANDONING
ABANDONMENT
ABANDONMENTS
ABANDONS
ABAPICAL
ABAS
ABASE
ABASED
ABASEDLY
ABASEMENT
ABASEMENTS
ABASER
ABASERS
ABASES
ABASH
ABASHED
ABASHEDLY
ABASHES
ABASHING
ABASHMENT
ABASHMENTS
ABASIA
ABASIAS
ABASING
ABATABLE
ABATE
ABATED
ABATEMENT
ABATEMENTS
ABATER
ABATERS
ABATES
ABATING
ABATIS
ABATISES
ABATOR
ABATORS
ABATTIS
ABATTISES
ABATTOIR
ABATTOIRS
ABAXIAL
ABAXILE
ABAYA
ABAYAS
ABBA
ABBACIES
ABBACY
ABBAS
ABBATIAL
ABBE
ABBES
ABBESS
ABBESSES
ABBEY
ABBEYS
ABBOT
ABBOTCIES
ABBOTCY
ABBOTS
ABBOTSHIP
ABBOTSHIPS
ABBREVIATE
ABBREVIATED
ABBREVIATES
ABBREVIATING
ABBREVIATION
ABBREVIATIONS
ABBREVIATOR
ABBREVIATORS
ABCOULOMB
ABCOULOMBS
ABDICABLE
ABDICATE
ABDICATED
ABDICATES
ABDICATING
ABDICATION
ABDICATIONS
ABDICATOR
ABDICATORS
ABDOMEN
ABDOMENS
ABDOMINA
ABDOMINAL
ABDOMINALLY
ABDOMINALS
ABDUCE
ABDUCED
ABDUCENS
ABDUCENT
ABDUCENTES
ABDUCES
ABDUCING
ABDUCT
ABDUCTED
ABDUCTEE
ABDUCTEES
ABDUCTING
ABDUCTION
ABDUCTIONS
ABDUCTOR
ABDUCTORES
ABDUCTORS
ABDUCTS
ABEAM
ABECEDARIAN
ABECEDARIANS
ABED
ABEGGING
ABELE
ABELES
ABELIA
ABELIAN
ABELIAS
ABELMOSK
ABELMOSKS
ABERRANCE
ABERRANCES
ABERRANCIES
ABERRANCY
ABERRANT
ABERRANTLY
ABERRANTS
ABERRATED
ABERRATION
ABERRATIONAL
ABERRATIONS
ABET
ABETMENT
ABETMENTS
ABETS
ABETTAL
ABETTALS
ABETTED
ABETTER
ABETTERS
ABETTING
ABETTOR
ABETTORS
ABEYANCE
ABEYANCES
ABEYANCIES
ABEYANCY
ABEYANT
ABFARAD
ABFARADS
ABHENRIES
ABHENRY
ABHENRYS
ABHOR
ABHORRED
ABHORRENCE
ABHORRENCES
ABHORRENT
ABHORRENTLY
ABHORRER
ABHORRERS
ABHORRING
ABHORS
ABIDANCE
ABIDANCES
ABIDE
ABIDED
ABIDER
ABIDERS
ABIDES
ABIDING
ABIDINGLY
ABIGAIL
ABIGAILS
ABILITIES
ABILITY
ABIOGENESES
ABIOGENESIS
ABIOGENIC
ABIOGENICALLY
ABIOGENIST
ABIOGENISTS
ABIOLOGICAL
ABIOSES
ABIOSIS
ABIOTIC
ABIOTICALLY
ABJECT
ABJECTION
ABJECTIONS
ABJECTLY
ABJECTNESS
ABJECTNESSES
ABJURATION
ABJURATIONS
ABJURE
ABJURED
ABJURER
ABJURERS
ABJURES
ABJURING
ABLATE
ABLATED
ABLATES
ABLATING
ABLATION
ABLATIONS
ABLATIVE
ABLATIVELY
ABLATIVES
ABLATOR
ABLATORS
ABLAUT
ABLAUTS
ABLAZE
ABLE
ABLED
ABLEGATE
ABLEGATES
ABLEISM
ABLEISMS
ABLEIST
ABLEISTS
ABLER
ABLES
ABLEST
ABLINGS
ABLINS
ABLOOM
ABLUENT
ABLUENTS
ABLUSH
ABLUTED
ABLUTION
ABLUTIONARY
ABLUTIONS
ABLY
ABMHO
ABMHOS
ABNEGATE
ABNEGATED
ABNEGATES
ABNEGATING
ABNEGATION
ABNEGATIONS
ABNEGATOR
ABNEGATORS
ABNORMAL
ABNORMALITIES
ABNORMALITY
ABNORMALLY
ABNORMALS
ABNORMITIES
ABNORMITY
ABO
ABOARD
ABODE
ABODED
ABODES
ABODING
ABOHM
ABOHMS
ABOIDEAU
ABOIDEAUS
ABOIDEAUX
ABOIL
ABOITEAU
ABOITEAUS
ABOITEAUX
ABOLISH
ABOLISHABLE
ABOLISHED
ABOLISHER
ABOLISHERS
ABOLISHES
ABOLISHING
ABOLISHMENT
ABOLISHMENTS
ABOLITION
ABOLITIONARY
ABOLITIONISM
ABOLITIONISMS
ABOLITIONIST
ABOLITIONISTS
ABOLITIONS
ABOLLA
ABOLLAE
ABOMA
ABOMAS
ABOMASA
ABOMASAL
ABOMASI
ABOMASUM
ABOMASUS
ABOMINABLE
ABOMINABLY
ABOMINATE
ABOMINATED
ABOMINATES
ABOMINATING
ABOMINATION
ABOMINATIONS
ABOMINATOR
ABOMINATORS
ABOON
ABORAL
ABORALLY
ABORIGINAL
ABORIGINALLY
ABORIGINALS
ABORIGINE
ABORIGINES
ABORNING
ABORT
ABORTED
ABORTER
ABORTERS
ABORTIFACIENT
ABORTIFACIENTS
ABORTING
ABORTION
ABORTIONIST
ABORTIONISTS
ABORTIONS
ABORTIVE
ABORTIVELY
ABORTIVENESS
ABORTIVENESSES
ABORTS
ABORTUS
ABORTUSES
ABOS
ABOUGHT
ABOULIA
ABOULIAS
ABOULIC
ABOUND
ABOUNDED
ABOUNDING
ABOUNDS
ABOUT
ABOVE
ABOVEBOARD
ABOVEGROUND
ABOVES
ABRACADABRA
ABRACADABRAS
ABRACHIA
ABRACHIAS
ABRADABLE
ABRADANT
ABRADANTS
ABRADE
ABRADED
ABRADER
ABRADERS
ABRADES
ABRADING
ABRASION
ABRASIONS
ABRASIVE
ABRASIVELY
ABRASIVENESS
ABRASIVENESSES
ABRASIVES
ABREACT
ABREACTED
ABREACTING
ABREACTION
ABREACTIONS
ABREACTS
ABREAST
ABRI
ABRIDGE
ABRIDGED
ABRIDGEMENT
ABRIDGEMENTS
ABRIDGER
ABRIDGERS
ABRIDGES
ABRIDGING
ABRIDGMENT
ABRIDGMENTS
ABRIS
ABROACH
ABROAD
ABROGABLE
ABROGATE
ABROGATED
ABROGATES
ABROGATING
ABROGATION
ABROGATIONS
ABROGATOR
ABROGATORS
ABROSIA
ABROSIAS
ABRUPT
ABRUPTER
ABRUPTEST
ABRUPTION
ABRUPTIONS
ABRUPTLY
ABRUPTNESS
ABRUPTNESSES
ABS
ABSCESS
ABSCESSED
ABSCESSES
ABSCESSING
ABSCISE
ABSCISED
ABSCISES
ABSCISIN
ABSCISING
ABSCISINS
ABSCISSA
ABSCISSAE
ABSCISSAS
ABSCISSION
ABSCISSIONS
ABSCOND
ABSCONDED
ABSCONDER
ABSCONDERS
ABSCONDING
ABSCONDS
ABSEIL
ABSEILED
ABSEILING
ABSEILS
ABSENCE
ABSENCES
ABSENT
ABSENTED
ABSENTEE
ABSENTEEISM
ABSENTEEISMS
ABSENTEES
ABSENTER
ABSENTERS
ABSENTING
ABSENTLY
ABSENTMINDED
ABSENTMINDEDLY
ABSENTS
ABSINTH
ABSINTHE
ABSINTHES
ABSINTHS
ABSOLUTE
ABSOLUTELY
ABSOLUTENESS
ABSOLUTENESSES
ABSOLUTER
ABSOLUTES
ABSOLUTEST
ABSOLUTION
ABSOLUTIONS
ABSOLUTISM
ABSOLUTISMS
ABSOLUTIST
ABSOLUTISTIC

*)