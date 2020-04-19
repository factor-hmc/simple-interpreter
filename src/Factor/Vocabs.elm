module Factor.Vocabs exposing (vocabs)

import Dict


vocabs =
    [ ( "kernel"
      , (-- primitives in `kernel.factor`, here manually defined in
         -- terms of other simpler primitives
         [ ( "dupd", "[ dup ] dip" )
         , ( "nip", "[ drop ] dip" )
         , ( "over", "[ dup ] dip swap" )
         , ( "pick", "[ over ] dip swap" )
         , ( "rot", "[ swap ] dip swap" )
         , ( "-rot", "rot rot" )
         , ( "swapd", "[ swap ] dip" )
         , ( "2drop", "drop drop" )
         , ( "2dup", "over over" )
         , ( "2nip", "[ 2drop ] dip" )
         , ( "3drop", "drop drop drop" )
         , ( "3dup", "pick pick pick" )
         , ( "4drop", "drop drop drop drop" )
         , ( "4dup", "reach reach reach reach" )
         ]
            |> List.map (\( name, def ) -> ": " ++ name ++ " ( -- ) " ++ def ++ " ;")
            |> String.join "\n"
        )
            ++ """

! Stack stuff
: 2over ( x y z -- x y z x y ) pick pick ; inline

: ? ( ? true false -- true/false )
    rot [ drop ] [ nip ] if ; inline
! Single branch
: unless ( -- ) ! ( ..a ? false: ( ..a -- ..a ) -- ..a )
    swap [ drop ] [ call ] if ; inline

: when ( -- ) ! ( ..a ? true: ( ..a -- ..a ) -- ..a )
    swap [ call ] [ drop ] if ; inline

! Anaphoric
: if* ( -- ) ! ( ..a ? true: ( ..a ? -- ..b ) false: ( ..a -- ..b ) -- ..b )
    pick [ drop call ] [ 2nip call ] if ; inline

: when* ( -- ) ! ( ..a ? true: ( ..a ? -- ..a ) -- ..a )
    over [ call ] [ 2drop ] if ; inline

: unless* ( -- ) ! ( ..a ? false: ( ..a -- ..a x ) -- ..a x )
    over [ drop ] [ nip call ] if ; inline

! Default
: ?if ( -- ) ! ( ..a default cond true: ( ..a cond -- ..b ) false: ( ..a default -- ..b ) -- ..b )
    pick [ drop [ drop ] 2dip call ] [ 2nip call ] if ; inline

! Dippers.
! Not declared inline because the compiler special-cases them

! our online interpreter doesn't, so no
! : dip ( x quot -- x ) swap [ call ] dip ;

: 2dip ( x y quot -- x y ) swap [ dip ] dip ;

: 3dip ( x y z quot -- x y z ) swap [ 2dip ] dip ;

: 4dip ( w x y z quot -- w x y z ) swap [ 3dip ] dip ; inline

! Misfits
: tuck ( x y -- y x y ) dup -rot ; inline

: spin ( x y z -- z y x ) -rot swap ; inline

: rotd ( w x y z -- x y w z ) [ rot ] dip ; inline

: -rotd ( w x y z -- y w x z ) [ -rot ] dip ; inline

: roll ( w x y z -- x y z w ) rotd swap ; inline

: -roll ( w x y z -- z w x y ) swap -rotd ; inline

: nipd ( x y z -- y z ) [ nip ] dip ; inline

: overd ( x y z -- x y x z ) [ over ] dip ; inline

: pickd ( w x y z -- w x y w z ) [ pick ] dip ; inline

: 2nipd ( w x y z -- y z ) [ 2drop ] 2dip ; inline

: 3nipd ( v w x y z -- y z ) [ 3drop ] 2dip ; inline

: 3nip ( w x y z -- z ) 2nip nip ; inline

: 4nip ( v w x y z -- z ) 2nip 2nip ; inline

: 5nip ( u v w x y z -- z ) 3nip 2nip ; inline

: 5drop ( v w x y z -- ) 4drop drop ; inline

: reach ( w x y z -- w x y z w ) [ pick ] dip swap ; inline

! Keepers

! we have no support for this fancy stack effect parsing, so we'll
! just as a quick hack get rid of it

: keep ( -- ) ! ( ..a x quot: ( ..a x -- ..b ) -- ..b x )
    over [ call ] dip ; inline

: 2keep ( -- ) ! ( ..a x y quot: ( ..a x y -- ..b ) -- ..b x y )
    [ 2dup ] dip 2dip ; inline

: 3keep ( -- ) ! ( ..a x y z quot: ( ..a x y z -- ..b ) -- ..b x y z )
    [ 3dup ] dip 3dip ; inline

: 4keep ( -- ) ! ( ..a w x y z quot: ( ..a w x y z -- ..b ) -- ..b w x y z )
    [ 4dup ] dip 4dip ; inline

: keepd ( -- ) ! ( ..a x y quot: ( ..a x y -- ..b x ) -- ..b x )
    2keep drop ; inline

: keepdd ( -- ) ! ( ..a x y z quot: ( ..a x y z -- ..b x ) -- ..b x )
    3keep 2drop ; inline

: 2keepd ( -- ) ! ( ..a x y z quot: ( ..a x y z -- ..b x y ) -- ..b x y )
    3keep drop ; inline

! Cleavers
: bi ( x p q -- )
    [ keep ] dip call ; inline

: tri ( x p q r -- )
    [ [ keep ] dip keep ] dip call ; inline

! Double cleavers
: 2bi ( x y p q -- )
    [ 2keep ] dip call ; inline

: 2tri ( x y p q r -- )
    [ [ 2keep ] dip 2keep ] dip call ; inline

! Triple cleavers
: 3bi ( x y z p q -- )
    [ 3keep ] dip call ; inline

: 3tri ( x y z p q r -- )
    [ [ 3keep ] dip 3keep ] dip call ; inline

! Spreaders
: bi* ( x y p q -- )
    [ dip ] dip call ; inline

: tri* ( x y z p q r -- )
    [ [ 2dip ] dip dip ] dip call ; inline

! Double spreaders
: 2bi* ( w x y z p q -- )
    [ 2dip ] dip call ; inline

: 2tri* ( u v w x y z p q r -- )
    [ 4dip ] 2dip 2bi* ; inline

! Appliers
: bi@ ( x y quot -- )
    dup bi* ; inline

: tri@ ( x y z quot -- )
    dup dup tri* ; inline

! Double appliers
: 2bi@ ( w x y z quot -- )
    dup 2bi* ; inline

: 2tri@ ( u v w x y z quot -- )
    dup dup 2tri* ; inline

! Quotation building
: 2curry ( obj1 obj2 quot -- curried )
    curry curry ; inline

: 3curry ( obj1 obj2 obj3 quot -- curried )
    curry curry curry ; inline

: with ( param obj quot -- obj curried )
    swapd [ swapd call ] 2curry ; inline

: 2with ( param1 param2 obj quot -- obj curried )
    with with ; inline

: prepose ( quot1 quot2 -- composed )
    swap compose ; inline

! should be private, but the elm interpreter can't parse private
! blocks yet, so this is what we're stuck with

: currier ( quot -- quot' ) [ curry ] curry ; inline

: bi-curry ( x p q -- p' q' ) [ currier ] bi@ bi ; inline

: tri-curry ( x p q r -- p' q' r' ) [ currier ] tri@ tri ; inline

: bi-curry* ( x y p q -- p' q' ) [ currier ] bi@ bi* ; inline

: tri-curry* ( x y z p q r -- p' q' r' ) [ currier ] tri@ tri* ; inline

: bi-curry@ ( x y q -- p' q' ) currier bi@ ; inline

: tri-curry@ ( x y z q -- p' q' r' ) currier tri@ ; inline

! Booleans

: >boolean ( obj -- ? ) [ t ] [ f ] if ; inline

: not ( obj -- ? ) [ f ] [ t ] if ; inline

: and ( obj1 obj2 -- ? ) over ? ; inline

: or ( obj1 obj2 -- ? ) dupd ? ; inline

: xor ( obj1 obj2 -- ? ) [ f swap ? ] when* ; inline

: both? ( x y quot -- ? ) bi@ and ; inline

: either? ( x y quot -- ? ) bi@ or ; inline

: most ( x y quot -- z ) 2keep ? ; inline

! Loops
: loop ( -- ) ! ( ... pred: ( ... -- ... ? ) -- ... )
    [ call ] keep [ loop ] curry when ; inline

: do ( pred body -- pred body )
    dup 2dip ; inline

: while ( -- ) ! ( ..a pred: ( ..a -- ..b ? ) body: ( ..b -- ..a ) -- ..b )
    swap do compose [ loop ] curry when ; inline

: while* ( -- ) ! ( ..a pred: ( ..a -- ..b ? ) body: ( ..b ? -- ..a ) -- ..b )
    [ [ dup ] compose ] dip while drop ; inline

: until ( -- ) ! ( ..a pred: ( ..a -- ..b ? ) body: ( ..b -- ..a ) -- ..b )
    [ [ not ] compose ] dip while ; inline

"""
      )
    , ( "math"
      , """
: (each-integer) ( -- ) ! ( ... i n quot: ( ... i -- ... ) -- ... )
    2over < [
        [ nip call ] 3keep
        [ 1 + ] 2dip (each-integer)
    ] [
        3drop
    ] if ; inline ! recursive

: (find-integer) ( -- ) ! ( ... i n quot: ( ... i -- ... ? ) -- ... i/f )
    2over < [
        [ nip call ] 3keep roll
        [ 2drop ]
        [ [ 1 + ] 2dip (find-integer) ] if
    ] [
        3drop f
    ] if ; inline ! recursive

: (all-integers?) ( -- ) ! ( ... i n quot: ( ... i -- ... ? ) -- ... ? )
    2over < [
        [ nip call ] 3keep roll
        [ [ 1 + ] 2dip (all-integers?) ]
        [ 3drop f ] if
    ] [
        3drop t
    ] if ; inline ! recursive

: each-integer ( -- ) ! ( ... n quot: ( ... i -- ... ) -- ... )
    [ 0 ] 2dip (each-integer) ; inline

: times ( -- ) ! ( ... n quot: ( ... -- ... ) -- ... )
    [ drop ] prepose each-integer ; inline

: find-integer ( -- ) ! ( ... n quot: ( ... i -- ... ? ) -- ... i/f )
    [ 0 ] 2dip (find-integer) ; inline

: all-integers? ( -- ) ! ( ... n quot: ( ... i -- ... ? ) -- ... ? )
    [ 0 ] 2dip (all-integers?) ; inline

: find-last-integer ( -- ) ! ( ... n quot: ( ... i -- ... ? ) -- ... i/f )
    over 0 < [
        2drop f
    ] [
        [ call ] 2keep rot [
            drop
        ] [
            [ 1 - ] dip find-last-integer
        ] if
    ] if ; inline ! recursive
"""
      )
    ]
        |> Dict.fromList
