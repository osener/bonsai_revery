module Mouse_move = struct
  type t = Revery_UI.NodeEvents.mouseMoveEventParams =
    { mouseX : float
    ; mouseY : float
    ; ctrlKey : bool
    ; altKey : bool
    ; shiftKey : bool
    ; guiKey : bool
    }
end

module Mouse_button = struct
  type t = Revery_UI.NodeEvents.mouseButtonEventParams =
    { mouseX : float
    ; mouseY : float
    ; button : Revery_Core.MouseButton.t
    ; ctrlKey : bool
    ; altKey : bool
    ; shiftKey : bool
    ; guiKey : bool
    }
end

module Mouse_wheel = struct
  type t = Revery_UI.NodeEvents.mouseWheelEventParams =
    { deltaX : float
    ; deltaY : float
    ; ctrlKey : bool
    ; altKey : bool
    ; shiftKey : bool
    ; guiKey : bool
    }
end

module Text_input = struct
  type t = Revery_UI.NodeEvents.textInputEventParams = { text : string }
end

module Text_edit = struct
  type t = Revery_UI.NodeEvents.textEditEventParams =
    { text : string
    ; start : int
    ; length : int
    }
end

module Keyboard = struct
  type key_event_params = Revery_UI.NodeEvents.keyEventParams =
    { keycode : Revery_Core.Key.Keycode.t
    ; scancode : Revery_Core.Key.Scancode.t
    ; keymod : Revery_Core.Key.Keymod.t
    ; repeat : bool
    ; ctrlKey : bool
    ; altKey : bool
    ; shiftKey : bool
    ; guiKey : bool
    }

  type t =
    { key : Keyboard.t
    ; ctrl : bool
    ; shift : bool
    ; event_params : key_event_params
    }
end

module Dimensions_changed = Revery_UI.NodeEvents.DimensionsChangedEventParams

module Bounding_box_changed = struct
  type t = Revery_Math.BoundingBox2d.t
end
