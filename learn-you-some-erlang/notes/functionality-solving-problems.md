# Functionally Solving Problems

Source: https://learnyousomeerlang.com/functionally-solving-problems#rpn-calculator

### Reverse Polish Notation Calculator

- Polish notation: 2 + 2) / 5 would become (/ (+ 2 2) 5)
- If we decide to say + and / always take two arguments, then (/ (+ 2 2) 5) can simply be written as / + 2 2 5.
- The same example as above in RPN would be written 2 2 + 5 /. Other example expressions could be 9 _ 5 + 7 or 10 _ 2 _ (3 + 4) / 2 which get translated to 9 5 _ 7 + and 10 2 _ 3 4 + _ 2 /, respectively.
