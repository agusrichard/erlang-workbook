-module(kata).
-export([century/1]).

century(Year) when Year rem 100 == 0 -> Year div 100;
century(Year) -> (Year div 100) + 1.
