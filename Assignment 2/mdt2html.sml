exception Italic_Error
exception Bold_Error
exception Link_Error
exception Underline_Error
exception Heading_Error
exception Table_Error
exception Block_Quote_Error

fun italicstart (lineList) = 
    if(hd(lineList) = #"*") then boldstart(tl(lineList))
    else 
        let val (italic, continueList) = italicend (lineList);
        in ("<i>"^italic, continueList)
        end

and boldstart (lineList) = 
    if(hd(lineList) = #"*") then 
        let val (complete, continueList) = bolditalic (tl(lineList));
        in ("<b><i>"^complete, continueList)
        end
    else 
        let val (bold, continueList) = boldend (lineList);
        in ("<b>"^bold, continueList)
        end

and bolditalic (lineList) =
    if(hd(lineList) = #"*") then
        if(hd(tl(lineList)) = #"*") then
            if(hd(tl(tl(lineList))) = #"*") then ("</i></b>", tl(tl(tl(lineList))))
            else
                let val (continueLine, newList) = italicend(tl(tl(lineList)));
                in ("</b>"^continueLine, newList)
                end
        else
            let val (continueLine, newList) = boldend(tl(lineList));
            in ("</i>"^continueLine, newList)
            end
    else if (hd(lineList) = #"\n") then raise Bold_Error
    else
        let val (continueLine, newList) = bolditalic(tl(lineList));
        in (Char.toString(hd(lineList))^continueLine, newList)
        end

and boldend (lineList) = 
    if(hd(lineList) = #"*") then
        if(hd(tl(lineList)) = #"*") then
            ("</b>", tl(tl(lineList)))
        else raise Bold_Error
    else if (hd(lineList) = #"\n") then raise Bold_Error
    else 
        let val (continueLine, newList) = boldend(tl(lineList));
        in (Char.toString(hd(lineList))^continueLine, newList)
        end

and italicend (lineList) = 
    if(hd(lineList) = #"*") then
        if(hd(tl(lineList)) = #"*") then  
            let val (complete, continueList) = bolditalic(tl(tl(lineList)));
            in ("<b>"^complete, continueList)
            end
        else ("</i>", tl(lineList))
    else if (hd(lineList) = #"\n") then raise Italic_Error
    else 
        let val (continueLine, newList) = italicend(tl(lineList));
        in (Char.toString(hd(lineList))^continueLine, newList)
        end

and linkStart (lineList) = 
    if(hd(lineList) = #")") then ("",tl(lineList))
    else if (hd(lineList) = #"\n") then raise Link_Error
    else
        let val (link, continueList) = linkStart(tl(lineList));
        in (Char.toString(hd(lineList)) ^ link, continueList)
        end

and linkTextStart (lineList) = 
    if(hd(lineList) = #"]") then
        let val (link, continueList) = linkStart(tl(tl(lineList)));
        in ("", link, continueList)
        end
    else if (hd(lineList) = #"\n") then raise Link_Error
    else 
        let val (text, link, continueList) = linkTextStart(tl(lineList));
        in (Char.toString(hd(lineList))^text, link, continueList)
        end

and autoLink (lineList) = 
    if(hd(lineList) = #">") then ("", tl(lineList), false)
    else if (hd(lineList) = #"\n") then raise Link_Error
    else
        let val (link, continueList, linkortag) = autoLink(tl(lineList));
        in if(hd(lineList) = #".") then (Char.toString(hd(lineList))^link, continueList, true)
            else (Char.toString(hd(lineList))^link, continueList, linkortag)
        end

and underline (lineList) =
    if (hd(lineList) = #"\n") then raise Underline_Error
    else if (hd(lineList) = #"_") then ("", tl(lineList))
    else let val (underlined, remain) = underline (tl(lineList))
        in (Char.toString(hd(lineList))^underlined, remain)
        end

and plaintext (lineList, tableOrNot, listOrNot) =
    if(null lineList = true) then ""
    else if(hd(lineList) = #"$") then ""
    else if(hd(lineList) = #"*") then  
        let val (previous, nextString) = italicstart(tl(lineList));
        in previous^plaintext(nextString, tableOrNot, listOrNot)
        end
    else if(hd(lineList) = #"[") then
        let val (text, link, nextString) = linkTextStart(tl(lineList));
        in "<a href=\""^link^"\">"^text^"</a>"^plaintext(nextString, tableOrNot, listOrNot)
        end
    else if(hd(lineList) = #"<") then
        let val (link, nextString, linkortag) = autoLink(tl(lineList));
        in if (linkortag = true) then "<a href=\""^link^"\">"^link^"</a>"^plaintext(nextString, tableOrNot, listOrNot)
            else "<"^link^">"^plaintext(nextString, tableOrNot, listOrNot)
        end
    else if(hd(lineList) = #"_") then
        let val (underText, nextLine) = underline(tl(lineList))
        in "<u>"^underText^"</u>"^plaintext(nextLine, tableOrNot, listOrNot)
        end
    else if (tableOrNot = true andalso hd(lineList) = #"|") then "</TD><TD>"^plaintext (tl(lineList), tableOrNot, listOrNot)
    else if (tableOrNot = true andalso hd(lineList) = #"\n") then "</TD>" 
    else if (hd(lineList) = #"\n" andalso listOrNot = true) then 
        if(null(tl(lineList)) = false) then "\n"^plaintext(tl(lineList), tableOrNot, listOrNot)
        else ""
    else if (hd(lineList) = #"\n" andalso listOrNot = false) then ""
    else Char.toString(hd(lineList))^(plaintext(tl(lineList), tableOrNot, listOrNot))

and hd1 (lineList) =
    if(hd(lineList) = #"#") then hd2(tl(lineList))
    else "<h1> "^plaintext (lineList, false, false)^" </h1>\n"

and hd2 (lineList) =
    if(hd(lineList) = #"#") then hd3(tl(lineList))
    else "<h2> "^plaintext (lineList, false, false)^" </h2>\n"

and hd3 (lineList) =
    if(hd(lineList) = #"#") then hd4(tl(lineList))
    else "<h3> "^plaintext (lineList, false, false)^" </h3>\n"

and hd4 (lineList) =
    if(hd(lineList) = #"#") then hd5(tl(lineList))
    else "<h4> "^plaintext (lineList, false, false)^" </h4>\n"

and hd5 (lineList) =
    if(hd(lineList) = #"#") then hd6(tl(lineList))
    else "<h5> "^plaintext (lineList, false, false)^" </h5>\n"

and hd6 (lineList) =
    if(hd(lineList) = #"#") then raise Heading_Error
    else "<h6> "^plaintext (lineList, false, false)^" </h6>\n"

and handleNewLine (mdfile) =
    let val newLine = TextIO.inputLine mdfile;
    in
        case newLine  of
        NONE => ([#"$"], false)
      | SOME newLine => 
        let val lineList = explode(newLine);
        in if (hd(lineList) = #"\n") then
            let val (nextLine, boolval) = handleNewLine(mdfile);
            in (nextLine, true)
            end
            else (lineList, false)
        end
    end

and codeBlock (lineList, mdfile, listornot) = 
    if (hd(lineList) = #"$") then ("", lineList)
    else if (hd(lineList) = #" " andalso hd(tl(lineList)) = #" " andalso hd(tl(tl(lineList))) = #" " andalso hd(tl(tl(tl(lineList)))) = #" " 
            andalso listornot = false) then
        let 
            val (nextLine, para) = handleNewLine (mdfile)
            val (coded, newLine) = codeBlock (nextLine, mdfile, listornot)
        in 
            if(para = true) then (implode(tl(tl(tl(tl(lineList)))))^"\n"^coded, newLine)
            else (implode(tl(tl(tl(tl(lineList)))))^" "^coded, newLine)
        end
    else if (listornot = true andalso hd(lineList) = #" " andalso hd(tl(lineList)) = #" " andalso hd(tl(tl(lineList))) = #" " andalso hd(tl(tl(tl(lineList)))) = #" "
        andalso hd(tl(tl(tl(tl(lineList))))) = #" " andalso hd(tl(tl(tl(tl(tl(lineList)))))) = #" " andalso hd(tl(tl(tl(tl(tl(tl(lineList))))))) = #" " 
        andalso hd(tl(tl(tl(tl(tl(tl(tl(lineList)))))))) = #" ") then
        let 
            val (nextLine, para) = handleNewLine (mdfile)
            val (coded, newLine) = codeBlock (nextLine, mdfile, listornot)
        in 
            if(para = true) then (implode(tl(tl(tl(tl(tl(tl(tl(tl(lineList)))))))))^"\n"^coded, newLine)
            else (implode(tl(tl(tl(tl(tl(tl(tl(tl(lineList)))))))))^" "^coded, newLine)
        end
    else ("", lineList)

and unlistTextHandler (listItem, lineList, mdfile) =
    if(hd(lineList) = #"\n") then 
        let val (newLine, para) = handleNewLine(mdfile);
            val text = plaintext(explode(listItem^"\n"), false, true)
        in  if (para = true) then ("<p>"^text^"</p>\n", newLine)
            else if (hd(newLine) = #"-" orelse hd(newLine) = #"$") then (text, newLine)
            else unlistTextHandler(listItem^" "^Char.toString(hd(newLine)), tl(newLine), mdfile)
        end
    else if(hd(lineList) = #" " andalso hd(tl(lineList)) = #" " andalso hd(tl(tl(lineList))) = #" " andalso hd(tl(tl(tl(lineList)))) = #" "
        andalso hd(tl(tl(tl(tl(lineList))))) = #" " andalso hd(tl(tl(tl(tl(tl(lineList)))))) = #" " andalso hd(tl(tl(tl(tl(tl(tl(lineList))))))) = #" " 
        andalso hd(tl(tl(tl(tl(tl(tl(tl(lineList)))))))) = #" ") then
        let val (coded, newLine) = codeBlock (lineList, mdfile, true) 
        in unlistTextHandler(listItem^"\n<pre><code>"^coded^"</pre></code>\n", newLine, mdfile)
        end
    else unlistTextHandler(listItem^Char.toString(hd(lineList)), tl(lineList), mdfile)

and unorderedListStart(lineList, mdfile) = 
    if(hd(lineList) = #"-") then 
        let val (listItem, newLine) = unlistTextHandler("",tl(tl(lineList)), mdfile)
        in "<li>"^listItem^"</li>\n"^unorderedListStart(newLine, mdfile) 
        end 
    else ""

and numHandler (lineList) = 
    if(hd(lineList) = #".") then
        if(hd(tl(lineList)) = #" ") then (false, tl(tl(lineList)))
        else (true, lineList)
    else if (hd(lineList) = #"\\") then (true, lineList)
    else if (hd(lineList) = #"\n") then (true, lineList)
    else numHandler(tl(lineList))

and paragraph (lineList, mdfile, paraLine) =
    let 
        val converted = plaintext(lineList, false, false);
        val (newLine, para) = handleNewLine(mdfile);
    in
        if(para = true orelse hd(newLine) = #"$") then ("<p>"^paraLine^" "^converted^"</p>\n", newLine)
        else paragraph(newLine, mdfile, paraLine^" "^converted)
    end

and tableRow (mdfile) =
    let val newLine = TextIO.inputLine mdfile;
    in
        case newLine  of
        NONE => raise Table_Error
      | SOME newLine => 
        let val lineList = explode(newLine);
        in  if(hd(lineList) = #">" andalso hd(tl(lineList)) = #">") then "" 
            else "<TR><TD>"^plaintext(lineList, true, false)^"</TR>\n"^tableRow(mdfile)
        end
    end

and orlistTextHandler (listItem, lineList, mdfile) =
    if(hd(lineList) = #"\n") then 
        let val (newLine, para) = handleNewLine(mdfile);
            val text = plaintext(explode(listItem^"\n"), false, true)
        in  if (para = true) then ("<p>"^text^"</p>", newLine)
            else if(hd(newLine) = #" " andalso hd(tl(newLine)) = #" " andalso hd(tl(tl(newLine))) = #" " andalso hd(tl(tl(tl(newLine)))) = #" "
                andalso hd(tl(tl(tl(tl(newLine))))) = #" " andalso hd(tl(tl(tl(tl(tl(newLine)))))) = #" " andalso hd(tl(tl(tl(tl(tl(tl(newLine))))))) = #" " 
                andalso hd(tl(tl(tl(tl(tl(tl(tl(newLine)))))))) = #" ") then
                let val (coded, newLine) = codeBlock (newLine, mdfile, true) 
                in if (Char.ord (hd newLine) > 47  andalso Char.ord (hd newLine) < 58) then
                    (text^"\n<pre><code>"^coded^"</pre></code>\n", newLine)
                else orlistTextHandler(listItem^"\n<pre><code>"^coded^"</pre></code>\n", newLine, mdfile)
                end
            else if (Char.ord (hd lineList) > 47  andalso Char.ord (hd lineList) < 58) then
                let val (numornot, numLine) = numHandler(newLine)
                in if (numornot = false) then (text, newLine)
                else orlistTextHandler(listItem^" "^Char.toString(hd(newLine)), tl(numLine), mdfile)
                end
            else (text, newLine)
        end
    else if(hd(lineList) = #" " andalso hd(tl(lineList)) = #" " andalso hd(tl(tl(lineList))) = #" " andalso hd(tl(tl(tl(lineList)))) = #" "
        andalso hd(tl(tl(tl(tl(lineList))))) = #" " andalso hd(tl(tl(tl(tl(tl(lineList)))))) = #" " andalso hd(tl(tl(tl(tl(tl(tl(lineList))))))) = #" " 
        andalso hd(tl(tl(tl(tl(tl(tl(tl(lineList)))))))) = #" ") then
        let val (coded, newLine) = codeBlock (lineList, mdfile, true) 
        in orlistTextHandler(listItem^"\n<pre><code>"^coded^"</pre></code>\n", newLine, mdfile)
        end
    else orlistTextHandler(listItem^Char.toString(hd(lineList)), tl(lineList), mdfile)

and orderedListStart (lineList, mdfile) = 
    if(Char.ord (hd lineList) > 47  andalso Char.ord (hd lineList) < 58) then
        let val (numornot, numLine) = numHandler (lineList)
        in
            if (numornot = false) then
                let 
                    val (listItem, newLine) = orlistTextHandler("",numLine, mdfile)
                    val (after, lastLine) = orderedListStart(newLine, mdfile)
                in ("<li>"^listItem^"</li>\n"^after, lastLine)
                end
            else ("",lineList)
        end
    else ("",lineList)

and blockQuoteChecker (lineList, num) = 
    if (hd(lineList) = #">") then blockQuoteChecker(tl(lineList), num+1)
    else (num, lineList)

and blockQuoteNewLine (mdfile) = 
    let val newLine = TextIO.inputLine mdfile
    in
        case newLine  of
        NONE => [#"$"]
        | SOME newLine => explode(newLine)
    end

and blockQuoteEnder (num) =
    if (num = 1) then ""
    else if (num > 1) then "</blockquote>"^blockQuoteEnder(num-1)
    else raise Block_Quote_Error

and blockQuote (lineList, mdfile, num) = 
    let val (newNum, text) = blockQuoteChecker (lineList, 0)
    in 
        if (newNum = 0) then (blockQuoteEnder(num), text)
        else if(newNum = num) then
            let val newLine = blockQuoteNewLine(mdfile)
                val (blocked, lastLine) = blockQuote (newLine, mdfile, num)
            in (implode(text)^blocked, lastLine)
            end
        else if (newNum = num - 1) then
            if(hd(text) = #"\n") then   
                let val newLine = blockQuoteNewLine(mdfile)
                    val (blocked, lastLine) = blockQuote (newLine, mdfile, num-1)
                in ("</blockquote>\n"^blocked, lastLine)
                end
            else 
                let val newLine = blockQuoteNewLine(mdfile)
                    val (blocked, lastLine) = blockQuote (newLine, mdfile, num)
                in (implode(text)^blocked, lastLine)
                end
        else if (newNum = num + 1) then
            let val newLine = blockQuoteNewLine(mdfile)
                val (blocked, lastLine) = blockQuote (newLine, mdfile, num+1)
            in ("<blockquote>"^implode(text)^blocked, lastLine)
            end
        else raise Block_Quote_Error
    end

and textHandler (lineList, mdfile) =
    if(hd(lineList) = #"$") then ""
    else if (hd(lineList) = #"#") then hd1(tl(lineList))
    else if (hd(lineList) = #"<" andalso hd(tl(lineList)) = #"<") then
        let val table = tableRow(mdfile)
        in "\n<p><CENTER><TABLE border=\"1\">\n"^table^"</TABLE></CENTER></p>\n"
        end
    else if (hd(lineList) = #">") then
        let val (blockQ, newLine) = blockQuote(lineList, mdfile, 1)
        in "<blockquote>"^blockQ^"</blockquote>\n"^textHandler(newLine, mdfile)
        end
    else if (hd(lineList) = #" " andalso hd(tl(lineList)) = #" " andalso hd(tl(tl(lineList))) = #" " andalso hd(tl(tl(tl(lineList)))) = #" ") then
        let val (coded, newLine) = codeBlock (lineList, mdfile, false) 
        in "<pre><code>"^coded^"</pre></code>\n"^textHandler(newLine, mdfile)
        end
    else if (Char.ord (hd lineList) > 47  andalso Char.ord (hd lineList) < 58) then
        let val (numornot, newLine) = numHandler (lineList)
        in  if(numornot = false) then
                let val (list, newLine) = orderedListStart(lineList, mdfile)
                in "<ol>\n"^list^"</ol>\n"^textHandler(newLine, mdfile)
                end
            else
                let val (converted, newLine) = paragraph(lineList, mdfile, "")
                in converted^textHandler(newLine, mdfile)
                end
        end
    else if (hd(lineList) = #"-") then
        if (hd(tl(lineList)) = #"-") then
            if (hd(tl(tl(lineList))) = #"-") then
                if (hd(tl(tl(tl(lineList)))) = #"\n") then "<hr>\n"
                else 
                    let val (converted, newLine) = paragraph(lineList, mdfile, "")
                    in converted^textHandler(newLine, mdfile)
                    end
            else
                let val (converted, newLine) = paragraph(lineList, mdfile, "")
                in converted^textHandler(newLine, mdfile)
                end
        else if (hd(tl(lineList)) = #" ") then "<ul>\n"^unorderedListStart(lineList, mdfile)^"</ul>\n"
        else
            let val (converted, newLine) = paragraph(lineList, mdfile, "")
            in converted^textHandler(newLine, mdfile)
            end
    else
        let val (converted, newLine) = paragraph(lineList, mdfile, "")
        in converted^textHandler(newLine, mdfile)
        end

fun linebyline (mdfile) = 
    let val newLine = TextIO.inputLine mdfile;
    in
        case newLine  of
        NONE => ""
      | SOME newLine => 
        let val lineList = explode(newLine);
        in textHandler(lineList, mdfile)^linebyline(mdfile)
        end
    end

fun mdt2html (filename) = 
    let
        val mdfile = TextIO.openIn filename;
        val htmlfilename = substring(filename,0,size(filename)-4)^".html";
        val htmlfile = TextIO.openOut htmlfilename;
    in
        (TextIO.output(htmlfile, "<html>\n"^linebyline(mdfile)^"</html>");
        TextIO.closeOut htmlfile;
        TextIO.closeIn mdfile)
    end;