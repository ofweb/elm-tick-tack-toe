module Main exposing (..)

import Color as Colors
import Element exposing (Element, Grid, el, empty, grid, layout, text)
import Element.Attributes exposing (center, padding, px, spacing, verticalCenter)
import Element.Events exposing (onClick)
import Html exposing (Html)
import List
import List.Extra as ListE
import Maybe.Extra as MaybeE
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


-- MODEL


type Player
    = X
    | O


type alias Cell =
    { player : Player, x : Int, y : Int }


type alias Pieces =
    List Cell


type alias Pos a =
    { a | x : Int, y : Int }


type alias Board a =
    { a | pieces : Pieces, selected : Maybe Cell }


type alias Model =
    { pieces : Pieces, turn : Player, selected : Maybe Cell, winner : Maybe Player, error : Maybe String }


initialModel : Model
initialModel =
    { pieces = [ { x = 0, y = 0, player = O }, { x = 2, y = 2, player = O } ], turn = X, selected = Just { x = 1, y = 1, player = X }, winner = Nothing, error = Nothing }


setPos : Pos a -> Cell -> Cell
setPos pos cell =
    { cell | x = pos.x, y = pos.y }


posEq : Pos a -> Pos b -> Bool
posEq pos pos2 =
    pos.x == pos2.x && pos.y == pos2.y


playerEq : Player -> Cell -> Bool
playerEq player cell =
    case player of
        X ->
            case cell.player of
                X ->
                    True

                O ->
                    False

        O ->
            case cell.player of
                X ->
                    False

                O ->
                    True


getCell : Pos a -> { b | pieces : Pieces } -> Maybe Cell
getCell pos model =
    List.head (List.filter (posEq pos) model.pieces)


removeCell : Pos b -> { a | pieces : Pieces } -> { a | pieces : Pieces }
removeCell pos model =
    { model | pieces = List.filter (\c -> not (posEq c pos)) model.pieces }


getSelected : Board b -> Pos a -> Maybe Cell
getSelected model pos =
    MaybeE.filter (posEq pos) model.selected


isCellEmpty : Board b -> Pos a -> Bool
isCellEmpty board pos =
    MaybeE.isJust (MaybeE.orElse (getSelected board pos) (getCell pos board))


pieceCount : Board b -> Int
pieceCount board =
    List.length board.pieces
        + (if MaybeE.isJust board.selected then
            1
           else
            0
          )



-- UPDATE


type Msg
    = AddPiece Cell
    | MovePiece Cell
    | SelectPiece Cell
    | ShowError Error


type Error
    = SelectOpponentCell
    | SelectEmptyCell
    | MoveToOccupiedCell
    | AddToOwnCell
    | AddTOOpponentCell


showError : Error -> String
showError err =
    case err of
        SelectOpponentCell ->
            "That cell contains one of your oponents pieces, please select one of your own pieces to move"

        SelectEmptyCell ->
            "That cell is empty, please select one of your own pieces to move"

        MoveToOccupiedCell ->
            "That cell is occupied, please select an empty cell to move your selected piece to"

        AddToOwnCell ->
            "That cell contains on of your own pieces, please select an empty cell to add your next piece to"

        AddTOOpponentCell ->
            "That cell contains one of your oponents pieces, please select an empty cell to add your next piece to"


update : Msg -> Model -> Model
update msg m =
    let
        model =
            { m | error = Nothing }
    in
    case msg of
        AddPiece c ->
            { model | pieces = c :: model.pieces }

        MovePiece p ->
            case model.selected of
                Just cell ->
                    { model | selected = Nothing, pieces = setPos p cell :: model.pieces }

                Nothing ->
                    model

        SelectPiece p ->
            { model | selected = getCell p model, pieces = (removeCell p model).pieces }

        ShowError s ->
            { model | error = Just s }


clickEvent : Model -> Maybe Cell -> Pos a -> Msg
clickEvent model mCell pos =
    if pieceCount model == 6 then
        let
            selected =
                MaybeE.isJust model.selected

            ownPiece =
                MaybeE.isJust (MaybeE.filter (playerEq model.turn) mCell) && MaybeE.isJust mCell
        in
        case ( selected, ownPiece, mCell ) of
            ( True, _, Nothing ) ->
                MovePiece { x = pos.x, y = pos.y, player = model.turn }

            ( True, _, Just _ ) ->
                ShowError MoveToOccupiedCell

            ( False, True, Just cell ) ->
                SelectPiece cell

            ( False, _, Nothing ) ->
                ShowError SelectEmptyCell

            ( False, False, Just _ ) ->
                ShowError SelectOpponentCell
    else
        let
            ownPiece =
                MaybeE.isJust (MaybeE.filter (playerEq model.turn) mCell) && MaybeE.isJust mCell
        in
        case ( ownPiece, mCell ) of
            ( False, Nothing ) ->
                AddPiece { x = pos.x, y = pos.y, player = model.turn }

            ( False, _ ) ->
                ShowError AddTOOpponentCell

            ( True, _ ) ->
                ShowError AddToOwnCell



-- VIEW


type Styles
    = NoStyle
    | CellStyle CellStyles


type CellStyles
    = Clickable
    | Error
    | Selected


cellBaseSheet : List (Property Styles variation)
cellBaseSheet =
    [ Border.all 1
    , Border.solid
    , Border.rounded 5
    , Font.size 60
    ]


cellHoverStyle : CellStyles -> Property Styles variation
cellHoverStyle cellStyle =
    case cellStyle of
        Clickable ->
            hover [ Color.background Colors.lightBlue ]

        Error ->
            hover [ Color.background Colors.lightRed ]

        Selected ->
            hover [ Color.background Colors.lightGray ]


sheet : StyleSheet Styles variation
sheet =
    Style.styleSheet
        [ style NoStyle []
        , style CellStyle
            Clickable
            (cellHoverStyle Clickable :: cellBaseSheet)
        , style CellStyle
            Selected
            (cellHoverStyle Selected
                :: cellBaseSheet
            )
        , style CellStyle
            Error
            (cellHoverStyle Error :: cellBaseSheet)
        ]


getCellStyle : Msg -> Pos a -> Board b -> Styles
getCellStyle msg pos model =
    let
        selected =
            getSelected model pos
    in
    if MaybeE.isJust selected then
        CellStyle Selected
    else
        case msg of
            AddPiece cell ->
                CellStyle Clickable

            MovePiece cell ->
                CellStyle Clickable

            SelectPiece cell ->
                CellStyle Clickable

            ShowError _ ->
                CellStyle Error


view : Model -> Html Msg
view model =
    Element.layout sheet <|
        viewBoard model


viewBoard : Model -> Element Styles variation Msg
viewBoard model =
    el
        NoStyle
        [ center, verticalCenter ]
        (grid
            NoStyle
            [ spacing 10 ]
            { rows = List.repeat 3 (px 100)
            , columns = List.repeat 3 (px 100)
            , cells = viewCells (clickEvent model) model
            }
        )


viewCells : (Maybe Cell -> Pos a -> Msg) -> Board b -> List (Element.OnGrid (Element Styles variation Msg))
viewCells getMsg model =
    List.map
        (\pos ->
            let
                msg =
                    getMsg pos

                cellStyle =
                    getCellStyle msg pos model
            in
            Maybe.withDefault
                (viewEmptyCell msg cellStyle pos)
                (MaybeE.orElse
                    (Maybe.map (viewCell msg cellStyle) (getSelected model pos))
                    (Maybe.map (viewCell msg cellStyle) (getCell pos model))
                )
        )
        List.map
        (\[ x, y ] -> { x = x, y = y })
        (ListE.cartesianProduct
            [ [ 0, 1, 2 ], [ 0, 1, 2 ] ]
        )


viewCell : Msg -> Styles -> Cell -> Element.OnGrid (Element Styles variation Msg)
viewCell msg styles cell =
    Element.cell
        { start = ( cell.x, cell.y )
        , width = 1
        , height = 1
        , content = el styles [ onClick msg, center, verticalCenter, padding 20 ] (viewPlayer cell.player)
        }


viewEmptyCell : Msg -> Styles -> Pos a -> Element.OnGrid (Element Styles variation Msg)
viewEmptyCell msg styles pos =
    Element.cell { start = ( pos.x, pos.y ), width = 1, height = 1, content = el styles [ onClick msg ] empty }


viewPlayer : Player -> Element Styles variation Msg
viewPlayer player =
    case player of
        X ->
            text "X"

        O ->
            text "O"


main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
