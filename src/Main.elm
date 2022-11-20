module Main exposing (main)

import Browser
import Css exposing (..)
import Css.Transitions exposing (easeInOut, easeOut, transition)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (classList, css)
import Html.Styled.Events exposing (onClick)
import Process exposing (sleep)
import Random exposing (Generator)
import Task
import Time exposing (..)


type alias Model =
    { currentSequence : List Int
    , userSequence : List Int
    , activeButton : Maybe Int
    , currentLevel : Int
    , animatedButton : Maybe Int
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { currentSequence = []
      , userSequence = []
      , activeButton = Nothing
      , currentLevel = 0
      , animatedButton = Nothing
      }
    , Cmd.none
    )


type Msg
    = Reset
    | StartGame
    | RandomResult Int
    | AddToUserSequence Int
    | SetActiveButton Int
    | ClearActiveButton
    | ClearAnimatedButton


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


showSequence : List Int -> String
showSequence sequence =
    sequence
        |> List.map String.fromInt
        |> String.concat


countScore : Int -> Int
countScore level =
    level * 100


sequenceLengthFromLevel : Int -> Int
sequenceLengthFromLevel level =
    level + 1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            let
                cmd =
                    Random.generate RandomResult (Random.int 0 8)

                newModel =
                    { model
                        | userSequence = []
                        , currentLevel = model.currentLevel + 1
                    }
            in
            ( newModel
            , if model.currentLevel == 0 then
                Cmd.batch [ cmd, cmd ]

              else
                cmd
            )

        RandomResult n ->
            let
                newSequence =
                    model.currentSequence ++ [ n ]

                cmds =
                    newSequence
                        |> List.map SetActiveButton
                        |> List.intersperse ClearActiveButton
                        |> (\xs -> xs ++ [ ClearActiveButton ])
                        |> List.indexedMap (\i m -> delay (toFloat i * 200) m)
            in
            ( { model | currentSequence = newSequence }
            , if List.length newSequence == sequenceLengthFromLevel model.currentLevel then
                Cmd.batch cmds

              else
                Cmd.none
            )

        AddToUserSequence n ->
            if gameIsInProgress model || (model.userSequence == [] && model.currentSequence /= []) then
                ( { model
                    | userSequence = model.userSequence ++ [ n ]
                    , animatedButton = Just n
                  }
                , delay 250 ClearAnimatedButton
                )

            else
                ( model, Cmd.none )

        Reset ->
            initialModel ()

        SetActiveButton n ->
            ( { model | activeButton = Just n }, Cmd.none )

        ClearActiveButton ->
            ( { model | activeButton = Nothing }, Cmd.none )

        ClearAnimatedButton ->
            ( { model | animatedButton = Nothing }, Cmd.none )


gameStyle =
    [ marginRight auto
    , marginLeft auto
    , height (pct 100)
    , textAlign center
    ]


gameState model =
    h1
        []
        [ if gameHasNotStartedYet model then
            text "Game hasn't started yet."

          else if userWon model then
            text "User wins! Next level!"

          else if gameIsInProgress model then
            text ("Level: " ++ String.fromInt model.currentLevel)

          else
            text "You lost..."
        ]


points model =
    h2 [ css [ fontSize (px 20), paddingBottom (px 20), fontWeight bold, fontFamilies [ "Verdana" ] ] ]
        [ if gameHasNotStartedYet model then
            text "Score: 0"

          else if userWon model then
            text ("Score: " ++ String.fromInt (countScore model.currentLevel))

          else
            text ("Score: " ++ String.fromInt (countScore (model.currentLevel - 1)))
        ]


makeButton : Model -> Int -> Html Msg
makeButton model idx =
    let
        style_ =
            if model.activeButton == Just idx then
                activeButtonStyle

            else
                buttonStyle

        attrs =
            [ classList
                [ ( "animate__animated", True )
                , ( "animate__faster", True )
                , ( "animate__backInRight", model.activeButton == Nothing && model.currentLevel == 0 )
                , ( "animate__jello", model.animatedButton == Just idx || model.activeButton == Just idx )
                , ( "playingButton", True )
                , ( "startButtons", model.animatedButton == Nothing && model.activeButton == Nothing )
                ]
            , onClick (AddToUserSequence idx)
            ]
    in
    button (attrs ++ style_) []


view : Model -> Html Msg
view model =
    div
        [ css
            [ position absolute
            , top (pct 50)
            , left (pct 50)
            , transform (translate2 (pct -50) (pct -50))
            ]
        ]
        [ div [ css gameStyle ]
            [ gameState model
            , points model
            , div [ classList [ ( "buttons", True ) ], css [ margin auto ] ] (List.map (makeButton model) (List.range 0 8))
            , br [] []
            , if gameIsInProgress model then
                button ([ classList [ ( "startButton", True ) ], onClick Reset ] ++ startButtonStyle) [ text "Restart" ]

              else if userWon model then
                button ([ classList [ ( "startButton", True ) ], onClick StartGame ] ++ startButtonStyle) [ text "Next Level" ]

              else if gameHasNotStartedYet model then
                button ([ classList [ ( "startButton", True ) ], onClick StartGame ] ++ startButtonStyle) [ text "Start game" ]

              else if userLost model then
                button ([ classList [ ( "startButton", True ) ], onClick Reset ] ++ startButtonStyle) [ text "Start again" ]

              else
                button ([ classList [ ( "startButton", True ) ], onClick Reset ] ++ startButtonStyle) [ text "Start game" ]
            ]
        ]


userWon : Model -> Bool
userWon model =
    model.currentSequence == model.userSequence && model.currentSequence /= []


userLost : Model -> Bool
userLost model =
    model.currentSequence /= [] && (model.userSequence /= model.currentSequence)


gameHasNotStartedYet : Model -> Bool
gameHasNotStartedYet model =
    model.currentSequence == [] && model.userSequence == []


gameIsInProgress : Model -> Bool
gameIsInProgress model =
    model.currentSequence /= [] && (model.userSequence == [] || (model.userSequence /= [] && String.startsWith (showSequence model.userSequence) (showSequence model.currentSequence) && (model.currentSequence /= model.userSequence)))


startButtonStyle : List (Attribute msg)
startButtonStyle =
    [ css
        [ width (px 150)
        , height (px 60)
        , margin (px 20)
        , cursor pointer
        , borderRadius (px 10)
        , borderColor (hex "DDDDDD")
        , backgroundColor (hex "7FDBFF")
        , color (hex "FFFFFF")
        , borderWidth (px 2)
        ]
    ]


buttonStyle : List (Attribute msg)
buttonStyle =
    [ css
        [ margin (px 10)
        , cursor pointer
        , borderRadius (px 20)
        , backgroundColor (hex "DDDDDD")
        , transform (scale 1)
        ]
    ]


activeButtonStyle : List (Attribute msg)
activeButtonStyle =
    List.singleton <|
        css
            [ margin (px 10)
            , cursor pointer
            , borderRadius (px 20)
            , backgroundColor (hex "0074D9")
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
