
# Open questions

## Exercise 4
Happy's handling of left-recursive vs right-recursive:
Left-recursive parsing is more efficient at parsing then right-recursive (it affects the stack size, which is kept on the heap and thus needs to be garbage collected). Due to the result of left-recusive being in a constant stack-space parser, whereas right-recursive rules require stack space proportional to the length of the list being parsed. This performance issue becomes more relevant when using larger sequences. When using left-recursion the result of parser has to be reversed before using.

Parser combinators:
parser combinators can handle more ambuigious parsers and Happy can't handle that.

left vs right recursive grammers:
Use left recursion rather than right recursion wherever possible. While not strictly a performance issue, this
affects the size of the parser stack, which is kept on the heap and thus needs to be garbage collected.

The only reason we used left recursion is that Happy is more efficient at parsing left-recursive rules; they result in a
constant stack-space parser, whereas right-recursive rules require stack space proportional to the length of the list being
parsed. This can be extremely important where long sequences are involved, for instance in automatically generated
output. 

One implication of using left recursion is that the resulting list comes out reversed, and you have to reverse it again to
get it in the original order.


How does this compare to the situation when using parser combinators:

With happy we can define which parsers are left or right associative. The %left or %right directive is followed by a list of terminals, and declares all these tokens to be left or right-associative respectively. The precedence of these tokens with respect to other tokens is established by the order of the %left and %right directives: earlier means lower precedence. A higher precedence causes an operator to bind more tightly; in our example above, because '*' has a higher precedence than '+', the expression 1 + 2 * 3 will parse as 1 + (2 * 3).

What happens when two operators have the same precedence? This is when the associativity comes into play. Operators specified as left associative will cause expressions like 1 + 2 - 3 to parse as (1 + 2) - 3, whereas right-associative operators would parse as 1 + (2 - 3). There is also a %nonassoc directive which indicates that the specified operators may not be used together. For example, if we add the comparison operators '>' and '<' to our grammar, then we would probably give their precedence as:

## Exercise 10
It does matter where the recursive call is in the command sequence. If the recursive is at the beginning of the command sequence, the stack doesn't grow as mush per recursive action. Since there are fewer commands put on the stack per recursive action than when it is at the end.