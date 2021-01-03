(** Utilities for pretty apps *)

type t = UI.Style.allProps

type angle = Revery.Math.Angle.t =
  | Degrees of float
  | Radians of float

type transform = UI.Transform.t =
  | RotateZ of angle
  | RotateY of angle
  | RotateX of angle
  | Rotate of angle
  | Scale of float
  | ScaleX of float
  | ScaleY of float
  | TranslateX of float
  | TranslateY of float

type text_wrap_type = Revery.TextWrapping.wrapType =
  | NoWrap
  | Wrap
  | WrapIgnoreWhitespace
  | WrapHyphenate

type flex_direction =
  Flex__LayoutTypes.Create(Revery_UI__Layout.Node)(Flex__FixedEncoding).flexDirection

let align_items : _ -> t = UI.Style.alignItems
let align_self : _ -> t = UI.Style.alignSelf
let background_color : Color.t -> t = UI.Style.backgroundColor
let border : color:Color.t -> width:int -> t = UI.Style.border
let border_bottom : color:Color.t -> width:int -> t = UI.Style.borderBottom
let border_horizontal : color:Color.t -> width:int -> t = UI.Style.borderHorizontal
let border_left : color:Color.t -> width:int -> t = UI.Style.borderLeft
let border_radius : _ -> t = UI.Style.borderRadius
let border_right : color:Color.t -> width:int -> t = UI.Style.borderRight
let border_top : color:Color.t -> width:int -> t = UI.Style.borderTop
let border_vertical : color:Color.t -> width:int -> t = UI.Style.borderVertical
let bottom : _ -> t = UI.Style.bottom

let box_shadow ~x_offset ~y_offset ~spread_radius ~blur_radius ~(color : Color.t) : t =
  UI.Style.boxShadow
    ~xOffset:x_offset
    ~yOffset:y_offset
    ~spreadRadius:spread_radius
    ~blurRadius:blur_radius
    ~color


let color : Color.t -> t = UI.Style.color

let cursor cursor_type =
  let cursor =
    let open Revery.MouseCursors in
    match cursor_type with
    | `Arrow -> arrow
    | `Text -> text
    | `Pointer -> pointer
    | `Crosshair -> crosshair
    | `Horizontal_resize -> horizontalResize
    | `Vertical_resize -> verticalResize in
  (UI.Style.cursor cursor : t)


let flex_basis : _ -> t = UI.Style.flexBasis
let flex_direction : _ -> t = UI.Style.flexDirection
let flex_grow : _ -> t = UI.Style.flexGrow
let flex_shrink : _ -> t = UI.Style.flexShrink
let flex_wrap : _ -> t = UI.Style.flexWrap
let height : _ -> t = UI.Style.height
let justify_content : _ -> t = UI.Style.justifyContent
let left : _ -> t = UI.Style.left
let line_height : _ -> t = UI.Style.lineHeight
let margin : _ -> t = UI.Style.margin
let margin2 : horizontal:int -> vertical:int -> t = UI.Style.margin2
let margin4 : top:int -> right:int -> bottom:int -> left:int -> t = UI.Style.margin4
let margin_bottom : _ -> t = UI.Style.marginBottom
let margin_horizontal : _ -> t = UI.Style.marginHorizontal
let margin_left : _ -> t = UI.Style.marginLeft
let margin_right : _ -> t = UI.Style.marginRight
let margin_top : _ -> t = UI.Style.marginTop
let margin_vertical : _ -> t = UI.Style.marginVertical
let max_height : _ -> t = UI.Style.maxHeight
let max_width : _ -> t = UI.Style.maxWidth
let min_height : _ -> t = UI.Style.minHeight
let min_width : _ -> t = UI.Style.minWidth
let opacity : _ -> t = UI.Style.opacity
let overflow : _ -> t = UI.Style.overflow
let padding : _ -> t = UI.Style.padding
let padding2 : horizontal:int -> vertical:int -> t = UI.Style.padding2
let padding4 : top:int -> right:int -> bottom:int -> left:int -> t = UI.Style.padding4
let padding_bottom : _ -> t = UI.Style.paddingBottom
let padding_horizontal : _ -> t = UI.Style.paddingHorizontal
let padding_left : _ -> t = UI.Style.paddingLeft
let padding_right : _ -> t = UI.Style.paddingRight
let padding_top : _ -> t = UI.Style.paddingTop
let padding_vertical : _ -> t = UI.Style.paddingVertical
let pointer_events : _ -> t = UI.Style.pointerEvents
let position : _ -> t = UI.Style.position
let right : _ -> t = UI.Style.right
let text_overflow : _ -> t = UI.Style.textOverflow
let text_wrap : text_wrap_type -> t = UI.Style.textWrap
let top : _ -> t = UI.Style.top
let transform : transform list -> t = UI.Style.transform
let width : _ -> t = UI.Style.width
