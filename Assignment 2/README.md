<center>

# Garv Nagori

## 2021CS10549
</center>

### Design Decisions

1. Bold/Italic/Underline can't run across multiple lines

        My name is *Garv Nagori
        CS* branch
   This code will throw an error

2. Space after # Headings is allowed

        #Garv Nagori
        # Garv Nagori
   Both are allowed

3. To differentiate between HTML Tags and Automatic Links, I have used a period. If a period is enclosed in <> then it is a link, otherwise it is a HTML Tag.

4. Tables and Lists can have italic, bold, underline. However, items in a table can't span multiple lines.

5. Ordered Lists does not necessarily have to start with 1. It can start from any number. Also, the character '.' is escaped if a number occurs at the beginning of a line.

6. Code Blocks have been implemented such that if it is indented 4 spaces from the left. In lists, the amount is 8 spaces.

7. Block Quotes have been implemented such that nesting is possible in this manner.

        > Block
        >> Quote
        > Example
   This code would translate to 

        <blockquote> Block
        <blockquote> Quote
        Example
        </blockquote>
        </blockquote>

   Also, you can't have an absolute difference of more than one ">" between lines.

        > Block Quote
        >>> Error

        >>> Block Quote
        > Error

   Both of these examples would throw an error.