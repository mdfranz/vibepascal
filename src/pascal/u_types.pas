unit u_types;

{$mode objfpc}{$H+}

interface

const
  MAX_ROOMS = 20;
  MAX_ITEMS = 24;
  INV_LOCATION = -1;
  MAX_HISTORY = 10;
  MAX_CARRY = 5;
  THIRST_LIMIT = 50;
  HORSE_THIRST_LIMIT = 40;
  DARK_TURN = 30;
  TWILIGHT_TURN = 20;
  SCORE_ROOM_VISIT = 5;
  SCORE_ITEM_PICKUP = 3;
  SCORE_NOTE_FOUND = 5;
  SCORE_PUMP_FIX = 20;
  SCORE_FIRST_FILL = 10;
  SCORE_LAMP_LIGHT = 5;
  SCORE_BOX_OPEN = 10;
  SCORE_OUTLAW_KILL = 15;
  SCORE_TELEGRAPH_FIX = 10;
  STREAM_ROOM_ID = 13;
  DESERT_ENTRY_ROOM_ID = 8;

type
  PRoom = ^TRoom;
  TRoom = record
    ID: Integer;
    Name: string;
    Description: string;
    North, South, East, West: PRoom;
  end;

  TItem = record
    Name: string;
    Description: string;
    Details: string;
    Location: Integer;
    IsTakeable: Boolean;
  end;

implementation

end.
