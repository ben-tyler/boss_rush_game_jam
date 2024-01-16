module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, h1, main_, text)
import Browser.Events as E
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Json.Decode as D
import String exposing (fromInt)
import String exposing (left)
import String exposing (right)
import Dict exposing (keys)
import Browser.Navigation exposing (Key)

-- Pick Up Items to fight with, you hit the exchange button to swap items

type Screen = Menu Float | Game

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = \msg model -> (update msg model, Cmd.none)
    , subscriptions = subscriptions
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ E.onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
    , E.onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
    , E.onAnimationFrameDelta TimeDelta
    , E.onVisibilityChange VisibilityChanged
    ]

initfloor = 
  [ { x = 50, y = 350, width = 700}
  , { x = 850, y = 350, width = 600}
  , { x = 1400, y = 250, width = 600}
  , { x = 2000, y = 350, width = 600}
  , { x = 2000, y = 50, width = 600}
  , { x = 2700, y = 350, width = 600}
  , { x = 3200, y = 350, width = 600}
  , { x = -500, y = 350, width = 400}
  ]




force = 100

init : () -> ( Model, Cmd Msg )
init _ =
  ( { keys = noKeys
    , player = initPlayer
    , camera = Camera 0 0 
    , floor = initfloor
    , boss = initBoss
    , screen = Game --Menu -1
    }
  , Cmd.batch
      [ 
      ]
  )

type alias Model =
  { keys : Keys
  , player : Player
  , camera : Camera
  , floor : List Floor
  , boss : Boss
  , screen : Screen
  }


type alias Keys =
  { up : Bool
  , left : Bool
  , down : Bool
  , right : Bool
  , space : Bool
  }



type Direction = Left | Right

type alias SwordData = 
  { strength : Float
  , speed : Int
  , size : Float
  }

type CoolDown = CoolDown Int
type AnimationTime = AnimationTime Int



type Weapon = 
  Sword SwordData

type WeaponControl = Holding Weapon CoolDown| Attacking Weapon AnimationTime

type Ailment = Good | Hit

type alias Player =
  { x   : Float
  , y   : Float
  , vx  : Float
  , vy  : Float
  , dir : Direction
  , standingOn : Maybe Floor
  , weaponControl : WeaponControl
  , ailment : Ailment
  }


type Decision 
  = MoveTo (Float, Float) 
  | NoDecision
  | JumpTo (Float, Float)

type alias Boss = 
  { x   : Float
  , y   : Float
  , vx  : Float
  , vy  : Float
  , dir : Direction
  , standingOn : Maybe Floor
  , descision : Decision
  , ailment : Ailment
  }

type alias Camera = 
  { x : Float
  , y : Float
  }

type alias Floor = 
  { x : Float
  , y : Float
  , width : Float
  }

type Collided = RightCollide CollisionData | LeftCollide CollisionData | TopCollide CollisionData | BottomCollide CollisionData

type alias CollisionData = 
  { x : Float
  , y : Float
  , w : Float
  , h : Float
  }

checkCollisions : CollisionData -> List CollisionData -> List Collided
checkCollisions collisionData nodesToCheck =
  let {x, y, w, h } = collisionData in
  nodesToCheck 
    |> List.filter (\data ->
        let {bx, by, bw, bh} = { bx = data.x, by = data.y, bw = data.w, bh = data.h} in 
        (x < bx + bw && x + w > bx) && (y < by + bh && y + h > by)
      ) 
    |> List.map (\item ->
        let (cx, cy) = (item.x + (item.w / 2), item.y + (item.h / 2)) in
        if x + w > cx then
            RightCollide item
        else if x < cx then
            LeftCollide item
        else if y < cy then
            TopCollide item
        else
            BottomCollide item
      )
    


--jump : Keys -> Player -> Player
jump keys player =
    if keys.up && player.vy == 0 then { player | vy = 2 } else player

--gravity : Float -> Float ->  Player -> Player
gravity dt floor player  =
    { player 
      | vy = if player.y < floor then player.vy - dt/force else 0 
      , vx = 
        case player.ailment of 
          Good -> 
            player.vx
          Hit -> 
            if player.vx > 0 && player.vx - dt/force <= 0 then 0
            else if player.vx < 0 && player.vx - dt/force >= 0 then 0
            else if player.vx > 0 then player.vx - dt/force 
            else if player.vx < 0 then player.vx + dt/force 
            else 0

      , ailment = 
        case player.ailment of
              Good -> Good
              Hit -> if player.vx == 0 then Good else Hit 
      
    }

--physics : Float -> Float -> Player  -> Player
physics dt floor player  =
    { player |
        x = player.x + dt * player.vx,
        y = min floor (player.y - dt * player.vy)
    }

--walk : Keys -> Player -> Player
walk keys speed player  =
  let {left, right} = keys in
  { player 
    | vx = if keys.left then -1/speed else if keys.right then 1/speed else 0
    , dir = if left then Left else if right then Right else player.dir
  }

initPlayer : Player
initPlayer = 
    { x = 50
    , y = 0
    , vx = 0
    , vy = 0
    , dir = Right
    , standingOn = Nothing
    , weaponControl = Holding (Sword {strength = 1, speed = 10, size = 50}) (CoolDown 0)
    , ailment = Good
    }

initBoss : Boss
initBoss =     
    { x = 200
    , y = 0
    , vx = 0
    , vy = 0
    , dir = Right
    , standingOn = Nothing
    , descision = NoDecision
    , ailment = Good
    }


noKeys : Keys
noKeys =
  Keys False False False False False


type Msg
  = KeyChanged Bool String
  | TimeDelta Float
  | Resized Float Float
  | VisibilityChanged E.Visibility

updateKeys : Bool -> String -> Keys -> Keys
updateKeys isDown key keys =
  case key of
    " "          -> { keys | space = isDown }
    "ArrowUp"    -> { keys | up    = isDown }
    "ArrowLeft"  -> { keys | left  = isDown }
    "ArrowDown"  -> { keys | down  = isDown }
    "ArrowRight" -> { keys | right = isDown }
    _            -> keys


standingOn  floor player = 
  checkCollisions 
    {x = player.x, y = player.y, w = 50, h = 50}
    (List.map ( \ i -> {x = i.x, y = i.y, w = i.width, h = 20})  floor)
    |> ( \ n -> 
      case n of 
        [] -> Nothing
        TopCollide c::_ -> 
          Just <| Floor c.x c.y c.w
        RightCollide c::_ -> 
          Just <| Floor c.x c.y c.w
        LeftCollide c::_ -> 
          Just <| Floor c.x c.y c.w
        BottomCollide c::_ -> 
          Just <| Floor c.x c.y c.w
      )
    |> ( \ g-> { player | standingOn = g } )

attack : Keys -> Player -> Player
attack keys player = 
  case player.weaponControl of 
    Holding (Sword data) (CoolDown 0) -> 
      if keys.space then
        { player | weaponControl = Attacking (Sword data) (AnimationTime <| data.speed) }
      else 
        player

    Holding (Sword data) (CoolDown cd) -> 
      { player | weaponControl = Holding (Sword data) (CoolDown <| cd - 1) }
      

    Attacking (Sword data) (AnimationTime t) ->
      if t == 0 then 
        { player | weaponControl = Holding (Sword data) (CoolDown <| data.speed) }
      else 
        { player | weaponControl = Attacking (Sword data) (AnimationTime <| t - 1) }


step : Float -> Model  -> Player -> Player
step dt model player =
  let
    floor = case player.standingOn of 
      Just c -> c.y 
      Nothing -> 99999
  in
    player
        |> gravity dt floor
        |> jump model.keys
        |> walk model.keys 2
        |> physics dt floor
        |> standingOn model.floor
        |> attack model.keys

keysleft : Keys
keysleft = { up  = False
       , left  = True
       , down  = False
       , right  = False
       , space = False
       }

keysUpleft : Keys
keysUpleft = { up  = True
       , left  = True
       , down  = False
       , right  = False
       , space = False
       }

keysright : Keys
keysright = { up  = False
       , left  = False
       , down  = False
       , right  = True
       , space = False
       }

keysUpRight : Keys
keysUpRight = { up  = True
       , left  = False
       , down  = False
       , right  = True
       , space = False
       }

inRange i1 i2 =
   i1 < i2 + 1 && i1 > i2 - 1


bossAI : Model -> Boss -> Boss
bossAI model boss = 
  let
  
    nextDecision = 
      case boss.descision of 
        NoDecision -> 
          case (model.player.standingOn, boss.standingOn) of
            (Just playerFloor, Just bossFloor) -> 
              if  playerFloor == bossFloor then 
                MoveTo (model.player.x, boss.y)
              else if inRange bossFloor.x boss.x then 
                JumpTo (playerFloor.width, boss.y - 300)
              else if inRange boss.x bossFloor.width then 
                JumpTo (playerFloor.x, boss.y - 300)
              else if bossFloor.x > playerFloor.x then 
                MoveTo (bossFloor.x, boss.y)
              else 
                MoveTo (bossFloor.width, boss.y)

            _ -> 
              boss.descision
      
        JumpTo (x, y) -> 
          if boss.x < x + 1 && boss.x > x - 1 then 
            NoDecision
          else 
            boss.descision


        MoveTo (x, y) -> 
          if boss.x < x + 1 && boss.x > x - 1 then 
            NoDecision
          else 
            boss.descision

  in 
  { boss | descision = nextDecision }

stepBoss : Float -> Model  -> Boss -> Boss
stepBoss dt model boss =
  let
    floor = case boss.standingOn of 
      Just c -> c.y 
      Nothing -> 99999

    keys = 
      case boss.descision of 
        MoveTo (x, y) -> 
          if boss.x > x then 
            keysleft
          else if boss.x < x then 
            keysright
          else
            noKeys

        JumpTo (x, y) -> 
          if boss.x > x then 
            keysUpleft
          else if boss.x < x then 
            keysUpRight
          else 
            noKeys


        _ -> 
          noKeys
            
  in
    boss
        |> gravity dt floor
        |> jump keys
        |> (\ p -> 
            case p.ailment of 
              Good -> walk keys 4 p
              Hit -> p
          )
        |> physics dt floor
        |> standingOn model.floor
        |> bossAI model
 
checkAttacks : Player -> Boss -> (Player, Boss)
checkAttacks player boss = 
  case player.weaponControl of 
    Attacking (Sword data) _ ->
      let 
        collides = 
          checkCollisions 
            (case player.dir of 
              Left -> 
                { x = player.x - 10
                , y  = player.y
                , w  = 50
                , h = 50
                }
              Right -> 
                { x = player.x + 20
                , y  = player.y
                , w  = 50
                , h = 50
                }
            )
            [ { x = boss.x
              , y  = boss.y
              , w  = 50
              , h = 50
              }
            ]


        hurtBoss = 
            case collides of 
              LeftCollide _::_ -> 
                { boss 
                  | vx = data.strength
                  , ailment = Hit
                  }

              RightCollide _::_ -> 
                { boss 
                  | vx = data.strength * -1
                  , ailment = Hit
                }

              _ -> 
                boss
      in
      (player, hurtBoss)

    _ -> 
      (player, boss)

update : Msg -> Model -> Model
update msg model =
  case msg of

    KeyChanged isDown key ->
      case model.screen of 
        Menu i ->
          { model | screen = Menu 1 }
            
        Game -> 
          { model | keys = updateKeys isDown key model.keys }

    TimeDelta dt ->
      case model.screen of 
        Menu i -> 
          {model | 
            screen = 
              if i > 100 then Game 
              else if i /= -1 then Menu (i + 0.4) 
              else Menu -1 

          }

        Game -> 
          let
            playerStep =  step dt model model.player 
            bossStep = stepBoss dt model model.boss 

            (player, boss) = checkAttacks playerStep bossStep
          in
          { model 
            | player = player
            , boss = boss
            , camera =  Camera (player.x  - 250) model.camera.y --if player.x > 200 then Camera (player.x  - 200 ) model.camera.y else model.camera
          }
          

    Resized width height ->
        model

    VisibilityChanged _ ->
      { model | keys = noKeys }


-- I have taken this time, the exchange is complete.

view : Model -> Html Msg
view model =
    main_ 
      [ Attr.style "position" "relative"
      , Attr.style "overflow" "hidden"
      , Attr.style "height" "400px"
      , Attr.style "width" "600px"]
      (case model.screen of 
        Menu i -> 
          [ Html.p [Attr.class "p-2 w-48"] [text "They made a calf in Horeb, and worshiped a molten image."]
          , Html.p [Attr.class "p-2 w-48"] [text "Thus they exchanged their glory for an image of a bull that eats grass."]
          , viewPlayer initPlayer (Camera -180 -350)
          , viewHead 200 (50 +  i) 100
          , Html.p [Attr.class "p-8"] [text "press any key to start..."]
          ]
        Game -> 
          [ viewPlayer model.player model.camera
          , case model.player.weaponControl of
              Attacking (Sword sword) (AnimationTime _) ->
                viewSword model.player model.camera sword
              _ -> div [] []
          , viewBoss model.boss model.camera
          , div [] <| List.map (viewFloor model.camera) model.floor 
          ]
      )

position : Float -> Float -> List (Html.Attribute Msg)
position x y = 
  [ Attr.style "position" "absolute"
  , Attr.style "top" <| (String.fromInt <| round y) ++ "px"
  , Attr.style "left" <| (String.fromInt <| round x) ++ "px"
  ] 

viewSword player camera sword = 
  case player.dir of 
    Left -> let {x, y} = { x = player.x - sword.size - camera.x, y = player.y - camera.y } in 
      div 
        (position (x) y)
        [ div [Attr.class "animate-pulse"] [div [Attr.class <| "triangle transform rotate-90 -mt-12 -ml-12"] []]
        ]


    Right ->  let {x, y} = { x = player.x + sword.size - camera.x, y = player.y - camera.y } in 
      div 
        (position (x) y)
        [ div [Attr.class "animate-pulse"] [div [Attr.class <| "triangle transform -rotate-90 -mt-12 ml-8"] []]
        ]

viewPlayer : Player -> Camera -> Html Msg
viewPlayer player camera = 
  let
    {x, y} = { x = player.x - camera.x, y = player.y - camera.y }
  in
  div 
    (position x y)
    [ div [Attr.class "h-10 w-10 bg-blue-500 rounded-full absolute top-[-40px]"] []
    , div [Attr.class "h-10 w-5 bg-blue-500 transform -skew-x-12 "] []
    ]

viewHead x y sizein  =
  let (size, sizeBy) = (String.fromInt sizein, String.fromInt (sizein * 2)) in
  div 
    (position x y) 
    [ div [Attr.class "animate-pulse"]
      [ div [Attr.class <| "triangle transform rotate-45 absolute top-[50px]"] []
      , div [Attr.class <| "triangle transform -rotate-45 absolute top-[50px] left-[50px]"] []
      , div [Attr.class <| "pentagon absolute top-[100px]"] []
      ]
    ]


viewBoss : Boss -> Camera -> Html Msg
viewBoss bos camera = 
  let
    {x, y} = { x = bos.x - camera.x, y = bos.y - camera.y }
  in
  div 
    (position x y)
    [ div [Attr.class "h-10 w-10 bg-red-500 rounded-full absolute top-[-40px]"] []
    , div [Attr.class "h-10 w-5 bg-red-500 transform -skew-x-12 animate-spin"] []
    ]


viewFloor : Camera -> Floor -> Html Msg
viewFloor camera floor  = 
  let
    {x, y} = { x = floor.x - camera.x, y = floor.y + 40 - camera.y }
  in
  div 
    (position x y)
    [ div 
      [ Attr.style "height" "20px"
      , Attr.style "width" ((floor.width |> round |> String.fromInt) ++  "px")
      , Attr.style "background-color" "grey"
      ] 
      []
    ]

 
