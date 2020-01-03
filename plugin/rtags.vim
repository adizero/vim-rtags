if exists('g:loaded_rtags')
    finish
end
let g:loaded_rtags = 1

if has('nvim') || (has('job') && has('channel'))
    let s:rtagsAsync = 1
    let s:job_cid = 0
    let s:jobs = {}
    let s:result_stdout = {}
    let s:result_handlers = {}
else
    let s:rtagsAsync = 0
endif

if has('python')
    let g:rtagsPy = 'python'
elseif has('python3')
    let g:rtagsPy = 'python3'
else
    echohl ErrorMsg | echomsg "[vim-rtags] Vim is missing python support" | echohl None
    finish
end



if !exists("g:rtagsRcCmd")
    let g:rtagsRcCmd = "rc"
endif

if !exists("g:rtagsInsertModeCompletion")
    let g:rtagsInsertModeCompletion = 1
endif

if !exists("g:rtagsRdmCmd")
    let g:rtagsRdmCmd = "rdm"
endif

if !exists("g:rtagsAutoLaunchRdm")
    let g:rtagsAutoLaunchRdm = 0
endif

if !exists("g:rtagsJumpStackMaxSize")
    let g:rtagsJumpStackMaxSize = 100
endif

if !exists("g:rtagsExcludeSysHeaders")
    let g:rtagsExcludeSysHeaders = 0
endif

let g:rtagsJumpStack = []

if !exists("g:rtagsUseLocationList")
    let g:rtagsUseLocationList = 1
endif

if !exists("g:rtagsUseDefaultMappings")
    let g:rtagsUseDefaultMappings = 1
endif

if !exists("g:rtagsUseDefaultSpecialMappings")
    let g:rtagsUseDefaultSpecialMappings = 1
endif

if !exists("g:rtagsMinCharsForCommandCompletion")
    let g:rtagsMinCharsForCommandCompletion = 4
endif

if !exists("g:rtagsMaxSearchResultWindowHeight")
    let g:rtagsMaxSearchResultWindowHeight = 10
endif

if g:rtagsAutoLaunchRdm
    call system(g:rtagsRcCmd." -w")
    if v:shell_error != 0
        call system(g:rtagsRdmCmd." --daemon > /dev/null")
    end
end

let g:SAME_WINDOW = 'same_window'
let g:H_SPLIT = 'hsplit'
let g:V_SPLIT = 'vsplit'
let g:NEW_TAB = 'tab'

let s:LOC_OPEN_OPTS = {
            \ g:SAME_WINDOW : '',
            \ g:H_SPLIT : ' ',
            \ g:V_SPLIT : 'vert',
            \ g:NEW_TAB : 'tab'
            \ }

function! rtags#IsRdmRunning()
    call system(g:rtagsRcCmd.' -w')
    if v:shell_error != 0
        echohl WarningMsg
        echomsg 'Rtags database manager (rdm) is not running'
        echohl None
        return 0
    end
    return 1
endfunction

function! rtags#Help()
    " show all <Leader>r* bindings
    execute 'map <Leader>r'
endfunction

function! rtags#CreateMapping(mapping, command)
    execute 'nnoremap ' . a:mapping . ' ' . ':' . 'if rtags#IsRdmRunning() \| call ' . a:command . ' \| endif<CR>'
endfunction

if g:rtagsUseDefaultMappings == 1
    " nnoremap <Leader>r? :if rtags#IsRdmRunning() \| call rtags#SymbolInfo() \| endif<CR>
    call rtags#CreateMapping('<Leader>r?', 'rtags#SymbolInfo()')
    call rtags#CreateMapping('<Leader>ri', 'rtags#AddIncludeForSymbolUnderCursor()')
    call rtags#CreateMapping('<Leader>rI', 'rtags#RemoveSuperfluousIncludesFromFile()')
    call rtags#CreateMapping('<Leader>rj', 'rtags#JumpTo(g:SAME_WINDOW)')
    call rtags#CreateMapping('<Leader>rJ', "rtags#JumpTo(g:SAME_WINDOW, { '--declaration-only' : '' })")
    call rtags#CreateMapping('<Leader>rS', 'rtags#JumpTo(g:H_SPLIT)')
    call rtags#CreateMapping('<Leader>rV', 'rtags#JumpTo(g:V_SPLIT)')
    call rtags#CreateMapping('<Leader>rT', 'rtags#JumpTo(g:NEW_TAB)')
    call rtags#CreateMapping('<Leader>rp', 'rtags#JumpToParent()')
    call rtags#CreateMapping('<Leader>rf', 'rtags#FindRefs()')
    call rtags#CreateMapping('<Leader>rF', 'rtags#FindRefsCallTree()')
    call rtags#CreateMapping('<Leader>rn', 'rtags#FindRefsByName(input("Pattern? ", "", "customlist,rtags#CompleteSymbols"))')
    call rtags#CreateMapping('<Leader>rs', 'rtags#FindSymbols(input("Pattern? ", "", "customlist,rtags#CompleteSymbols"))')
    call rtags#CreateMapping('<Leader>rr', 'rtags#ReindexFile()')
    call rtags#CreateMapping('<Leader>rl', 'rtags#ProjectList()')
    call rtags#CreateMapping('<Leader>rw', 'rtags#RenameSymbolUnderCursor()')
    call rtags#CreateMapping('<Leader>rv', 'rtags#FindVirtuals()')
    call rtags#CreateMapping('<Leader>rb', 'rtags#JumpBack()')
    call rtags#CreateMapping('<Leader>rh', 'rtags#ShowHierarchy()')
    call rtags#CreateMapping('<Leader>rC', 'rtags#FindSuperClasses()')
    call rtags#CreateMapping('<Leader>rc', 'rtags#FindSubClasses()')
    call rtags#CreateMapping('<Leader>rd', 'rtags#Diagnostics()')
    call rtags#CreateMapping('<Leader>r', 'rtags#Help()')
endif

if g:rtagsUseDefaultSpecialMappings == 1
    call rtags#CreateMapping('<C-S-Right>', 'rtags#JumpTo(g:SAME_WINDOW)')
    call rtags#CreateMapping('<C-S-Left>', 'rtags#JumpBack()')
    " call rtags#CreateMapping('g<LeftMouse> ', 'rtags#JumpTo(g:SAME_WINDOW)') " this has issues <LeftMouse> needs to be first and not nested within the if
    nnoremap g<LeftMouse> <LeftMouse>:if rtags#IsRdmRunning() \| call rtags#JumpTo(g:SAME_WINDOW) \| endif<CR>
    call rtags#CreateMapping('g<RightMouse> ', 'rtags#JumpBack()')
endif

let s:script_folder_path = escape( expand( '<sfile>:p:h' ), '\' )

function! rtags#InitPython()
    let s:pyInitScript = "
\ import vim;
\ script_folder = vim.eval('s:script_folder_path');
\ sys.path.insert(0, script_folder);
\ import vimrtags"

    exe g:rtagsPy." ".s:pyInitScript
endfunction

call rtags#InitPython()

"""
" Logging routine
"""
function! rtags#Log(message)
    if exists("g:rtagsLog")
        call writefile([string(a:message)], g:rtagsLog, "a")
    endif
endfunction

"
" Executes rc with given arguments and returns rc output
"
" param[in] args - dictionary of arguments
"-
" return output split by newline
function! rtags#ExecuteRC(args)
    let cmd = rtags#getRcCmd()

    " Give rdm unsaved file content, so that you don't have to save files
    " before each rc invocation.
    "" if exists('b:rtags_sent_content')
    ""     let content = join(getline(1, line('$')), "\n")
    ""     if b:rtags_sent_content != content
    ""         let unsaved_content = content
    ""     endif
    "" elseif &modified
        let unsaved_content = join(getline(1, line('$')), "\n")
    "" endif
    if exists('unsaved_content')
        let filename = expand("%")
        let output = system(printf("%s --unsaved-file=%s:%s -V %s", cmd, filename, strlen(unsaved_content), filename), unsaved_content)
        let b:rtags_sent_content = unsaved_content
    endif

    " prepare for the actual command invocation
    let l:filtering = ''
    for [key, value] in items(a:args)
        if key[0] !=# '|'
            let cmd .= " ".key
            if len(value) > 1
                let cmd .= " ".value
            endif
        else
            let l:filtering = key
        endif
    endfor
    let cmd .= l:filtering
    " echohl WarningMsg | echomsg 'Executing... ' . cmd | echohl None

    let output = system(cmd)
    if v:shell_error && len(output) > 0
        let output = substitute(output, '\n', '', '')
        echohl ErrorMsg | echomsg "[vim-rtags] Error: " . output | echohl None
        return []
    endif
    if output =~ '^Not indexed'
        echohl ErrorMsg | echomsg "[vim-rtags] Current file is not indexed!" | echohl None
        return []
    endif
    return split(output, '\n\+')
endfunction

function! rtags#CreateProject()

endfunction

function! s:EntryOrder(first, second)
    if a:first.filepath < a:second.filepath
        return -1
    elseif a:first.filepath > a:second.filepath
        return 1
    else
        if str2nr(a:first.lnum) < str2nr(a:second.lnum)
            return -1
        elseif str2nr(a:first.lnum) > str2nr(a:second.lnum)
            return 1
        else
            if str2nr(a:first.col) < str2nr(a:second.col)
                return -1
            elseif str2nr(a:first.col) > str2nr(a:second.col)
                return 1
            endif
        endif
    endif
endfunction

function! s:ReverseIntOrder(first, second)
    if a:first < a:second
        return 1
    elseif a:first > a:second
        return -1
    endif
    return 0
endfunction

function! s:AddFunctionOrClassOrNamespaceToSymbolReference(rest)
    " TODO improve, now the separator is simply <space>function:<space> - could collide with code comments or something
    let [l:text; l:f_location] = split(a:rest, '\s\+function: ')
    if empty(l:f_location)
        if l:text ==# ''
            return a:rest
        endif
        return l:text
    endif

    let [l:f_file, l:f_lnum, l:f_col] = rtags#parseSourceLocation(l:f_location[0])

    " TODO for now only sync exec !!!
    let l:args = {
                \ '-U' : l:f_location[0] }
    let l:f_text = ''
    let l:result = rtags#ExecuteRC(l:args)
    for l:result_line in l:result
        if match(l:result_line, 'SymbolName:') >= 0
            let l:f_text = l:result_line
            break
        endif
    endfor
    if l:f_text !=# ''
        let l:f_text = substitute(l:f_text, 'SymbolName: ', '', '')
        if match(l:f_text, 'class\|namespace\|struct') != 0
            let l:start_pos = 0
            while 1
                " echomsg l:f_text[l:start_pos:]
                let [l:str, l:start_pos, l:end_pos] = matchstrpos(l:f_text, '[A-Za-z_:]\+', l:start_pos)
                " echomsg l:str
                if l:start_pos < 0
                    break
                else
                    if match(l:str, ':operator\|^operator') >= 0
                        let l:pos = match(l:f_text, '(', l:end_pos)
                        if l:pos >= l:end_pos
                            if l:pos == l:end_pos
                                " exception for operator()
                                let l:pos = l:pos + 2
                            endif
                            let l:f_text = l:f_text[l:start_pos : l:pos - 1]
                            break
                        endif
                    elseif l:f_text[l:end_pos] ==# '('
                        let l:f_text = l:f_text[l:start_pos : l:end_pos - 1]
                        break
                    endif
                endif
                let l:start_pos = l:end_pos
            endwhile
        endif
        let l:f_text = '<<' . l:f_text . '>>'
    endif

    if l:f_text !=# ''
        return l:f_text . ' ' . l:text
    else
        return l:text
    endif
endfunction

"
" param[in] results - List of found locations by rc
" return locations - List of locations dict's recognizable by setloclist
"
function! rtags#ParseResults(results)
    let locations = []
    let nr = 1
    for record in a:results
        let [location; rest] = split(record, '\s\+')
        let [file, lnum, col] = rtags#parseSourceLocation(location)

        let entry = {}
        "        let entry.bufn = 0
        let entry.filename = substitute(file, getcwd().'/', '', 'g')
        let entry.filepath = file
        let entry.lnum = lnum
        "        let entry.pattern = ''
        let entry.col = col
        let entry.vcol = 0
        "        let entry.nr = nr
        let entry.text =  s:AddFunctionOrClassOrNamespaceToSymbolReference(join(rest, ' '))
        let entry.type = 'ref'

        call add(locations, entry)

        let nr = nr + 1
    endfor
    call sort(locations, 's:EntryOrder')
    return locations
endfunction

function! rtags#ExtractClassHierarchyLine(line)
    return substitute(a:line, '\v.*\s+(\S+:[0-9]+:[0-9]+:\s)', '\1', '')
endfunction

"
" Converts a class hierarchy of 'rc --class-hierarchy' like:
"
" Superclasses:
"   class Foo src/Foo.h:56:7: class Foo : public Bar {
"     class Bar src/Bar.h:46:7: class Bar : public Bas {
"       class Bas src/Bas.h:47:7: class Bas {
" Subclasses:
"   class Foo src/Foo.h:56:7: class Foo : public Bar {
"     class Foo2 src/Foo2.h:56:7: class Foo2 : public Foo {
"     class Foo3 src/Foo3.h:56:7: class Foo3 : public Foo {
"
" into the super classes:
"
" src/Foo.h:56:7: class Foo : public Bar {
" src/Bar.h:46:7: class Bar : public Bas {
" src/Bas.h:47:7: class Bas {
"
function! rtags#ExtractSuperClasses(results)
    let extracted = []
    for line in a:results
        if line == "Superclasses:"
            continue
        endif

        if line == "Subclasses:"
            break
        endif

        let extLine = rtags#ExtractClassHierarchyLine(line)
        call add(extracted, extLine)
    endfor
    return extracted
endfunction

"
" Converts a class hierarchy of 'rc --class-hierarchy' like:
"
" Superclasses:
"   class Foo src/Foo.h:56:7: class Foo : public Bar {
"     class Bar src/Bar.h:46:7: class Bar : public Bas {
"       class Bas src/Bas.h:47:7: class Bas {
" Subclasses:
"   class Foo src/Foo.h:56:7: class Foo : public Bar {
"     class Foo2 src/Foo2.h:56:7: class Foo2 : public Foo {
"     class Foo3 src/Foo3.h:56:7: class Foo3 : public Foo {
"
" into the sub classes:
"
" src/Foo.h:56:7: class Foo : public Bar {
" src/Foo2.h:56:7: class Foo2 : public Foo {
" src/Foo3.h:56:7: class Foo3 : public Foo {
"
function! rtags#ExtractSubClasses(results)
    let extracted = []
    let atSubClasses = 0
    for line in a:results
        if atSubClasses == 0
            if line == "Subclasses:"
                let atSubClasses = 1
            endif

            continue
        endif

        let extLine = rtags#ExtractClassHierarchyLine(line)
        call add(extracted, extLine)
    endfor
    return extracted
endfunction

"
" param[in] locations - List of locations, one per line
"
function! rtags#DisplayLocations(locations)
    let num_of_locations = len(a:locations)
    if g:rtagsUseLocationList == 1
        call setloclist(winnr(), a:locations)
        if num_of_locations > 0
            exe 'lopen '.min([g:rtagsMaxSearchResultWindowHeight, num_of_locations]) | set nowrap
        endif
    else
        call setqflist(a:locations)
        if num_of_locations > 0
            exe 'copen '.min([g:rtagsMaxSearchResultWindowHeight, num_of_locations]) | set nowrap
        endif
    endif
endfunction

"
" param[in] results - List of locations, one per line
"
" Format of each line: <path>,<line>\s<text>
function! rtags#DisplayResults(results)
    let locations = rtags#ParseResults(a:results)
    call rtags#DisplayLocations(locations)
endfunction

"
" Creates a tree viewer for references to a symbol
"
" param[in] results - List of locations, one per line
"
" Format of each line: <path>,<line>\s<text>\sfunction: <caller path>
function! rtags#ViewReferences(results)
    let cmd = g:rtagsMaxSearchResultWindowHeight . "new References"
    silent execute cmd
    setlocal noswapfile
    setlocal buftype=nowrite
    setlocal bufhidden=delete
    setlocal nowrap
    setlocal tw=0
    set ft=qf

    iabc <buffer>

    setlocal modifiable
    silent normal ggdG
    setlocal nomodifiable
    let b:rtagsLocations=[]
    call rtags#AddReferences(a:results, -1)
    setlocal modifiable
    silent normal ggdd
    setlocal nomodifiable

    let cpo_save = &cpo
    set cpo&vim
    nmap <buffer> <2-LeftMouse> <CR>
    nnoremap <buffer> p :call <SID>OpenReference(1)<cr>
    nnoremap <buffer> <cr> :call <SID>OpenReference(0)<cr>
    nnoremap <buffer> o :call <SID>ExpandReferences()<cr>
    nnoremap <buffer> <F11> :normal k<cr>:call <SID>OpenReference(1)<cr>
    nnoremap <buffer> <F12> :normal j<cr>:call <SID>OpenReference(1)<cr>
    let &cpo = cpo_save
endfunction

"
" Expands the callers of the reference on the current line.
"
function! s:ExpandReferences() " <<<
    let l:saved_cursor = getcurpos()
    let ln = line(".")
    let l = getline(ln)

    let l:nr = ln - 1
    let entry = b:rtagsLocations[l:nr]
    " for entry in b:rtagsLocations
    "    if entry.shown_text ==# l
            if entry.expanded == 0
                if !empty(entry.source)
                    let entry.expanded = 1
                    let location = entry.source
                    let args = {
                            \ '--containing-function-location' : '',
                            \ '-r' : location }
                    call rtags#ExecuteThen(args, [[function('rtags#AddReferences'), l:nr]])
                endif
            else
                let entry.expanded = 0
                " TODO remove below entries, where depth > current depth
                let l:current_depth = entry.depth
                let l:i = l:nr + 1
                setlocal modifiable
                while l:i < len(b:rtagsLocations)
                    " echomsg b:rtagsLocations[l:i].shown_text
                    if b:rtagsLocations[l:i].depth > l:current_depth
                        call remove(b:rtagsLocations, l:i)
                        execute ':' . string(l:i + 1) . 'd'
                    else
                        break
                    endif
                endwhile
                setlocal nomodifiable
            endif
    "        break
    "     endif
    "     let l:nr = l:nr + 1
    " endfor
    call setpos('.', l:saved_cursor)
    redraw
    " echomsg "restoring..." . l:saved_cursor[1] . ':' . l:saved_cursor[2]
endfunction " >>>

"
" Opens the reference for viewing in the previous window.
"
function! s:OpenReference(preview) " <<<
    let ln = line(".")
    let l = getline(ln)

    let l:nr = ln - 1
    let entry = b:rtagsLocations[l:nr]
    " for entry in b:rtagsLocations
    "     if entry.shown_text ==# l
            let jump_file = entry.filename
            let lnum = entry.lnum
            let col = entry.col
            wincmd p
            " Add location to the jumplist
            normal m'
            if rtags#jumpToLocation(jump_file, lnum, col)
                normal zz
            endif

            if a:preview == 1
                if has('autocmd')
                    let b:cursorline = &cursorline
                    let &cursorline = 1
                    augroup RTagsCallHierarchyBrowsing
                        autocmd! * <buffer>
                        autocmd WinEnter <buffer> let &cursorline = b:cursorline | autocmd! RTagsCallHierarchyBrowsing
                    augroup END
                endif

                wincmd p
            endif
    "         break
    "     endif
    " endfor
endfunction " >>>

"
" Adds the list of references below the targeted item in the reference
" viewer window.
"
" param[in] results - List of locations, one per line
" param[in] i - The index of the reference the added references are calling or -1
"
" Format of each line: <path>,<line>\s<text>\sfunction: <caller path>
function! rtags#AddReferences(results, i)
    let l:saved_cursor = getcurpos()
    " let ln = line(".")
    let nr = len(b:rtagsLocations)
    let depth = 0
    if a:i >= 0
        let depth = b:rtagsLocations[a:i].depth + 1
        " silent execute "normal! gg/#".a:i."$\<cr>"
    endif
    let prefix = repeat(" ", depth * 2)
    let l:new_locations=[]
    setlocal modifiable
    for record in a:results
        let [line; sourcefunc] = split(record, '\s\+function: ')
        let [location; rest] = split(line, '\s\+')
        let [file, lnum, col] = rtags#parseSourceLocation(location)
        let entry = {}
        let entry.filename = substitute(file, getcwd().'/', '', 'g')
        let entry.filepath = file
        let entry.lnum = lnum
        let entry.col = col
        let entry.vcol = 0
        let entry.text =  s:AddFunctionOrClassOrNamespaceToSymbolReference(join(rest, ' ') . ' function: ' . join(sourcefunc, ''))
        let entry.type = 'ref'
        let entry.depth = depth
        let entry.source = matchstr(sourcefunc, '[^\s]\+')
        let entry.expanded = 0
        let entry.nr = nr
        let entry.shown_text = prefix . substitute(entry.filename, '.*/', '', 'g').':'.entry.lnum.' '.entry.text
        call add(l:new_locations, entry)
        let nr = nr + 1
    endfor
    call sort(l:new_locations, 's:EntryOrder')
    let l:i = a:i
    for entry in l:new_locations
        " TODO Hide the index number of the entry - this is an implementation
        " detail that shouldn't be visible to the user.
        silent execute "normal! A\<cr>\<esc>i".entry.shown_text."\<esc>"
        if a:i < 0
            call add(b:rtagsLocations, entry)
        else
            call insert(b:rtagsLocations, entry, l:i + 1)
        endif
        let l:i = l:i + 1
    endfor
    setlocal nomodifiable
    " exec (":" . ln)
    call setpos('.', l:saved_cursor)
    redraw
endfunction

" Creates a viewer for hierarachy results
"
" param[in] results - List of class hierarchy
"
" Hierarchy references have format: <type> <name> <file>:<line>:<col>: <text>
"
function! rtags#ViewHierarchy(results)
    let cmd = g:rtagsMaxSearchResultWindowHeight . "new Hierarchy"
    silent execute cmd
    setlocal noswapfile
    setlocal buftype=nowrite
    setlocal bufhidden=delete
    setlocal nowrap
    setlocal tw=0

    iabc <buffer>

    setlocal modifiable
    silent normal ggdG
    for record in a:results
        silent execute "normal! A\<cr>\<esc>i".record."\<esc>"
    endfor
    silent normal ggdd
    setlocal nomodifiable

    let cpo_save = &cpo
    set cpo&vim
    nnoremap <buffer> <cr> :call <SID>OpenHierarchyLocation()<cr>
    let &cpo = cpo_save
endfunction

"
" Opens the location on the current line.
"
" Hierarchy references have format: <type> <name> <file>:<line>:<col>: <text>
"
function! s:OpenHierarchyLocation() " <<<
    let ln = line(".")
    let l = getline(ln)
    if l[0] == ' '
        let [type, name, location; rest] = split(l, '\s\+')
        let [jump_file, lnum, col; rest] = split(location, ':')
        wincmd j
        " Add location to the jumplist
        normal m'
        if rtags#jumpToLocation(jump_file, lnum, col)
            normal zz
        endif
    endif
endfunction " >>>

function! rtags#getRcCmd()
    let cmd = g:rtagsRcCmd
    let cmd .= " --absolute-path "
    if g:rtagsExcludeSysHeaders == 1
        return cmd." -H "
    endif
    return cmd
endfunction

function! rtags#getCurrentLocation()
    let [lnum, col] = getpos('.')[1:2]
    return printf("%s:%s:%s", expand("%:p"), lnum, col)
endfunction

function! rtags#SymbolInfoHandler(output)
    echo join(a:output, "\n")
    " TODO rework to preview window style instead of waiting for char input???
    " call getchar()
endfunction

function! rtags#SymbolInfo()
    call rtags#ExecuteNow({ '-U' : rtags#getCurrentLocation() }, [function('rtags#SymbolInfoHandler')])
endfunction

function! rtags#IncludeFileHandler(output, args)
    let l:symbol = a:args['symbol']
    let l:project_dir = a:args['project_dir']
    " echo join(a:output, "\n")
    "
    " prepare list of all include dirs
    let l:include_dirs = []

    let l:flag_output = rtags#ExecuteNow({ '--compilation-flags-only' : '', '--sources' : expand('%:p') }, [])
    for l:flag_line in l:flag_output
        let l:flag_line = substitute(l:flag_line, '-I ', '-I', 'g')
        let l:flag_line = substitute(l:flag_line, '-isystem ', '-isystem', 'g')
        " echomsg 'flag_line: ' . l:flag_line
        let l:flags = split(l:flag_line, ' ')
        for l:flag in l:flags
            let l:matchend = matchend(l:flag, '\C-I\|\C-isystem')
            if l:matchend > 0
                let l:dir = l:flag[l:matchend :]
                " resolve potential ./.. and similar things in path (transform to absolute path)
                let l:dir = fnamemodify(l:dir, ':p')
                call add(l:include_dirs, l:dir)
            endif
        endfor
    endfor
    " add main project dir if no include paths were specified
    if len(l:include_dirs) == 0
        let l:dir = fnamemodify(l:project_dir, ':p')
        call add(l:include_dirs, l:dir)
    endif
    " also add implicit directories (like current one)
    let l:current_dir = expand('%:p:h')
    " only do l:current_dir matching as last resort

    let l:candidate_list = []
    for l:line in a:output
        " skip class methods and constructors
        if match(l:line, '\sCXXMethod$\|\sCXXConstructor$') >= 0
            continue
        endif
        " skip forward class declarations of the searched symbol (search symbol is a class name in this case)
        if match(l:line, '\sClassDecl$') >= 0
            if match(l:line, 'class\s*' . l:symbol . '\s*;') >= 0
                continue
            endif
        elseif match(l:line, '\sStructDecl$') >= 0
            if match(l:line, 'struct\s*' . l:symbol . '\s*;') >= 0
                continue
            endif
        endif
        " cut everything after first colon (there must be a colon after filename)
        let l:first_colon = match(l:line, ':')
        if l:first_colon > 0
            let l:cut = l:line[: l:first_colon-1]
            " skip non header files (.c, .cc, .cpp, ... source files)
            if match(l:cut, '\.c.*$') >= 0
                continue
            endif

            " strip based on include directories (instead of just project dir)
            let l:longest_match = -1
            let l:dir = ''
            for l:include_dir in l:include_dirs
                " make all include dirs end with the trailing slash
                if l:include_dir[len(l:include_dir)-1] !=# '/'
                    let l:include_dir = l:include_dir . '/'
                endif
                let l:matchend = matchend(l:cut, '^' . l:include_dir)
                if l:matchend >= 0
                    if l:matchend > l:longest_match
                        let l:longest_match = l:matchend
                        let l:dir = l:include_dir
                    endif
                endif
            endfor

            " last resort current dir match (to not prefer these)
            if l:longest_match < 0
                let l:matchend = matchend(l:cut, '^' . l:current_dir)
                if l:matchend >= 0
                    " if l:matchend > l:longest_match
                        let l:longest_match = l:matchend
                        let l:dir = l:include_dir
                    " endif
                else
                    " no prefix match at all - ignore this full path (all include paths are usually relative to some -isystem or -I folder)
                    continue
                endif
            endif

            " strip dir prefix (the longest matching include dir)
            let l:matchend = matchend(l:cut, '^' . l:dir)
            if l:matchend >= 0
                let l:cut = l:cut[l:matchend :]
            endif

            " wrap in quotes or brackets
            " TODO(akocis): separate system and non-system header in a better way (maybe based on stripped prefix -isystem vs -I ?)
            let l:brackets = 1
            if match(l:cut, '\.h.*$') >= 0
                let l:brackets = 0
            endif
            let l:cut = (l:brackets ? '<' : '"') . l:cut . (l:brackets ? '>' : '"')
            " add to candidate list
            call add(l:candidate_list, l:cut)
        endif
    endfor
    " echo join(l:candidate_list, "\n")
    call vimrc#CodeIncludeOfferCandidates(l:symbol, l:candidate_list)
endfunction

function! rtags#AddIncludeForSymbol(symbol)
    " let l:filter_unwanted_clang_cursor_types = ''
    " let l:filter_unwanted_clang_cursor_types = '| grep -e "-\*- C++ -\*-$" -e ClassTemplate$ -e FunctionTemplate$ -e ClassDecl$ -e CXXMethod$ -e TypedefDecl$ -e StructDecl$ -e FunctionDecl$ -e "macro definition$"'
    let l:filter_unwanted_clang_cursor_types = '| grep -e "-\*- C++ -\*-$" -e ClassTemplate$ -e FunctionTemplate$ -e ClassDecl$ -e TypeAliasDecl$ -e TypedefDecl$ -e StructDecl$ -e FunctionDecl$ -e "macro definition$"'
    let l:args = {
                \ '-F' : a:symbol,
                \ '-G' : '',
                \ '--cursor-kind' : '',
                \ l:filter_unwanted_clang_cursor_types : '' }

    let l:output = rtags#ExecuteNow({ '--current-project' : '' }, [])
    let l:project_dir = ''
    if len(l:output) > 0
        let l:project_dir = l:output[0]
    endif
    call rtags#ExecuteNow(l:args, [[function('rtags#IncludeFileHandler'), { 'symbol' : a:symbol, 'project_dir' : l:project_dir }]])
endfunction

function! rtags#AddIncludeForSymbolUnderCursor()
    let l:WORDUnderCursor = expand('<cWORD>')
    let l:wordUnderCursor = expand('<cword>')
    " TODO improve namespaces and class support (maybe include :: into cword for expand call execution)
    return rtags#AddIncludeForSymbol(l:wordUnderCursor)
endfunction

function! rtags#RemoveIncludesHandler(output, args)
    let l:tmp_file = a:args['tmp_file']
    let l:current_file = a:args['current_file']
    let l:project_dir = a:args['project_dir']
    " echo join(a:output, "\n")

    " prepare map of include lines -> line no in current file
    let l:total = line('$')
    let l:i = 1
    let l:include_map = {}
    while l:i < l:total
        let l:current_line = getline(l:i)
        if l:current_line =~# '^\s*#include'
            let l:includeend = matchend(l:current_line, '#include\s*')
            if l:includeend >= 0
                let l:include_path = l:current_line[l:includeend :]
            else
                let l:include_path = l:current_line
            endif
            if l:include_path[0] ==# '"' || l:include_path[0] ==# '<'
                let l:include_path = l:include_path[1:]
            endif
            let l:length = len(l:include_path)
            if l:include_path[l:length-1] ==# '"' || l:include_path[l:length-1] ==# '>'
                let l:include_path = l:include_path[:l:length-2]
            endif
            let l:include_map[l:include_path] = l:i
            " echomsg 'include:' . l:include_path
        endif
        let l:i = l:i + 1
    endwhile

    let l:candidate_list = []
    for l:line in a:output
        " skip superfluous includes in other files than current one
        if match(l:line, l:tmp_file) < 0
            continue
        endif
        " skip error other than those the have 'for no reason' at the end of the line
        if match(l:line, 'for no reason$') < 0
            continue
        endif

        " cut everything after first colon (there must be a colon after filename)
        let l:includes_pos = match(l:line, '\sincludes\s.*\sfor no reason$')
        if l:includes_pos > 0
            let l:cut = l:line[l:includes_pos - 1 :]
            " strip includes prefix
            let l:matchend = matchend(l:cut, '\sincludes\s')
            if l:matchend >= 0
                let l:cut = l:cut[l:matchend :]
            endif
            " strip for not reason suffix
            let l:matchstart = match(l:cut, '\sfor no reason$')
            if l:matchstart > 0
                let l:cut = l:cut[: l:matchstart - 1]
            endif

            " now try to remove project / clang system from the include to be able to remove the #include line itself from the buffer
            "" let l:no_prefix = substitute(l:cut, '^' . l:project_dir, '', '')
            while !has_key(l:include_map, l:cut)
                " cuts the path from the from and tries to match in the include_map
                let l:slashend = matchend(l:cut, '/')
                if l:slashend >= 0
                    let l:cut = l:cut[l:slashend :]
                else
                    break
                endif
            endwhile

            if has_key(l:include_map, l:cut)
                " add to candidate list
                call add(l:candidate_list, l:include_map[l:cut])
            endif
        endif
    endfor
    " echo join(l:candidate_list, "\n")
    call sort(l:candidate_list, 's:ReverseIntOrder')
    for l:lineno in l:candidate_list
        let l:old_line = getline(l:lineno)

        " remove the include line from buffer
        execute l:lineno . 'delete _'

        echohl ModeMsg
        echomsg 'Removed superfluous include from line ' . string(l:lineno) . ': ' . l:old_line
        echohl None
    endfor
endfunction

function! rtags#RemoveSuperfluousIncludesFromFile()
    "" " save current buffer in tmp_file (rc does not work with --check-inludes on unsaved buffer, it does however work for other commands)
    "" " this also has problems with reindexing the temp file (would require correctly updating compile_commands.json and it is not worth it)
    "" let l:tempname = tempname()
    "" let l:tempdir = fnamemodify(l:tempname, ':p:h')
    "" let l:tmp_file = l:tempdir . '/' . expand('%:p:t')
    "" " this does not trigger buffer write autocmds (like linting)
    "" call writefile(getline(1,'$'), l:tmp_file, 'b')

    "" echomsg l:tmp_file
    "" let l:output = rtags#ExecuteNow({ '--reindex' : l:tmp_file }, [])

    if &modified
        echohl ModeMsg
        echomsg 'File has unsaved changes - these will be ignored when searching for superfluous includes'
        echohl None
    endif

    let l:extension = expand('%:e')
    if len(l:extension) == 0 || l:extension =~? 'h.*'
        echohl ModeMsg
        echomsg 'File looks to be a header file - check will not work, because it is based on compilation database (that does not include header files)'
        echohl None
    endif

    let l:current_file = expand('%:p')
    let l:tmp_file = l:current_file

    let l:args = {
                \ '--check-includes' : l:current_file }

    let l:output = rtags#ExecuteNow({ '--current-project' : '' }, [])
    let l:project_dir = ''
    if len(l:output) > 0
        let l:project_dir = l:output[0]
    endif
    call rtags#ExecuteNow(l:args, [[function('rtags#RemoveIncludesHandler'), { 'tmp_file' : l:tmp_file, 'current_file' : l:current_file, 'project_dir' : l:project_dir }]])
endfunction

function! rtags#cloneCurrentBuffer(type)
    if a:type == g:SAME_WINDOW
        return
    endif

    let [lnum, col] = getpos('.')[1:2]
    exec s:LOC_OPEN_OPTS[a:type]." new ".expand("%")
    call cursor(lnum, col)
endfunction

function! rtags#jumpToLocation(file, line, col)
    call rtags#saveLocation()
    return rtags#jumpToLocationInternal(a:file, a:line, a:col)
endfunction

function! rtags#jumpToLocationInternal(file, line, col)
    try
        if a:file != expand("%:p")
            exe "e ".a:file
        endif
        call cursor(a:line, a:col)
        return 1
    catch /.*/
        echohl ErrorMsg
        echomsg v:exception
        echohl None
        return 0
    endtry
endfunction

function! rtags#JumpToHandler(results, args)
    let results = a:results
    let open_opt = a:args['open_opt']
    if len(results) >= 0 && open_opt != g:SAME_WINDOW
        call rtags#cloneCurrentBuffer(open_opt)
    endif

    if len(results) > 1
        call rtags#DisplayResults(results)
    elseif len(results) == 1
        let [location; symbol_detail] = split(results[0], '\s\+')
        let [jump_file, lnum, col; rest] = split(location, ':')

        " Add location to the jumplist
        normal! m'
        if rtags#jumpToLocation(jump_file, lnum, col)
            normal! zz
        endif
    endif

endfunction

"
" JumpTo(open_type, ...)
"     open_type - Vim command used for opening desired location.
"     Allowed values:
"       * g:SAME_WINDOW
"       * g:H_SPLIT
"       * g:V_SPLIT
"       * g:NEW_TAB
"
"     a:1 - dictionary of additional arguments for 'rc'
"
function! rtags#JumpTo(open_opt, ...)
    let args = {}
    if a:0 > 0
        let args = a:1
    endif

    call extend(args, { '-f' : rtags#getCurrentLocation() })
    let results = rtags#ExecuteThen(args, [[function('rtags#JumpToHandler'), { 'open_opt' : a:open_opt }]])

endfunction

function! rtags#parseSourceLocation(string)
    let [location; symbol_detail] = split(a:string, '\s\+')
    let splittedLine = split(location, ':')
    if len(splittedLine) == 3
        let [jump_file, lnum, col; rest] = splittedLine
        " Must be a path, therefore leading / is compulsory
        if jump_file[0] == '/'
            return [jump_file, lnum, col]
        endif
    endif
    return ["","",""]
endfunction

function! rtags#saveLocation()
    let [lnum, col] = getpos('.')[1:2]
    call rtags#pushToStack([expand("%"), lnum, col])
endfunction

function! rtags#pushToStack(location)
    let jumpListLen = len(g:rtagsJumpStack)
    if jumpListLen > g:rtagsJumpStackMaxSize
        call remove(g:rtagsJumpStack, 0)
    endif
    call add(g:rtagsJumpStack, a:location)
endfunction

function! rtags#JumpBack()
    if len(g:rtagsJumpStack) > 0
        let [jump_file, lnum, col] = remove(g:rtagsJumpStack, -1)
        call rtags#jumpToLocationInternal(jump_file, lnum, col)
    else
        echo "rtags: jump stack is empty"
    endif
endfunction

function! rtags#JumpToParentHandler(results)
    let results = a:results
    for line in results
        let matched = matchend(line, "^Parent: ")
        if matched == -1
            continue
        endif
        let [jump_file, lnum, col] = rtags#parseSourceLocation(line[matched:-1])
        if !empty(jump_file)
            if a:0 > 0
                call rtags#cloneCurrentBuffer(a:1)
            endif

            " Add location to the jumplist
            normal! m'
            if rtags#jumpToLocation(jump_file, lnum, col)
                normal! zz
            endif
            return
        endif
    endfor
endfunction

function! rtags#JumpToParent(...)
    let args = {
                \ '-U' : rtags#getCurrentLocation(),
                \ '--symbol-info-include-parents' : '' }

    call rtags#ExecuteThen(args, [function('rtags#JumpToParentHandler')])
endfunction

function! s:GetCharacterUnderCursor()
    return matchstr(getline('.'), '\%' . col('.') . 'c.')
endfunction

function! rtags#RenameSymbolUnderCursorHandler(output)
    let locations = rtags#ParseResults(a:output)
    if len(locations) > 0
        let newName = input("Enter new name: ")
        let yesToAll = 0
        if !empty(newName)
            for loc in reverse(locations)
                if !rtags#jumpToLocationInternal(loc.filepath, loc.lnum, loc.col)
                    return
                endif
                normal! zv
                normal! zz
                redraw
                let choice = yesToAll
                if choice == 0
                    " TODO rework to make it work with airline on newly opened buffers somehow!!!!
                    " call getchar()

                    let location = loc.filepath.":".loc.lnum.":".loc.col
                    let choices = "&Yes\nYes to &All\n&No\n&Cancel"
                    let choice = confirm("Rename symbol at ".location, choices)
                endif
                if choice == 2
                    let choice = 1
                    let yesToAll = 1
                endif
                if choice == 1
                    " Special case for destructors
                    if s:GetCharacterUnderCursor() == '~'
                        normal! l
                    endif
                    exec "normal! ciw".newName."\<Esc>"
                    write!
                elseif choice == 4
                    return
                endif
            endfor
        endif
    endif
endfunction

function! rtags#RenameSymbolUnderCursor()
    let args = {
                \ '-e' : '',
                \ '-r' : rtags#getCurrentLocation(),
                \ '--rename' : '' }

    call rtags#ExecuteNow(args, [function('rtags#RenameSymbolUnderCursorHandler')])
endfunction

function! rtags#TempFile(job_cid)
    return '/tmp/neovim_async_rtags.tmp.' . getpid() . '.' . a:job_cid
endfunction

function! rtags#ExecuteRCAsync(args, handlers)
    let cmd = rtags#getRcCmd()

    " Give rdm unsaved file content, so that you don't have to save files
    " before each rc invocation.
    if exists('b:rtags_sent_content')
        let content = join(getline(1, line('$')), "\n")
        if b:rtags_sent_content != content
            let unsaved_content = content
        endif
    elseif &modified
        let unsaved_content = join(getline(1, line('$')), "\n")
    endif
    if exists('unsaved_content')
        let filename = expand("%")
        let output = system(printf("%s --unsaved-file=%s:%s -V %s", cmd, filename, strlen(unsaved_content), filename), unsaved_content)
        let b:rtags_sent_content = unsaved_content
    endif

    " prepare for the actual command invocation
    for [key, value] in items(a:args)
        let cmd .= " ".key
        if len(value) > 1
            let cmd .= " ".value
        endif
    endfor

    let s:callbacks = {
                \ 'on_exit' : function('rtags#HandleResults')
                \ }

    let s:job_cid = s:job_cid + 1
    " should have out+err redirection portable for various shells.
    if has('nvim')
        let cmd = cmd . ' >' . rtags#TempFile(s:job_cid) . ' 2>&1'
        let job = jobstart(cmd, s:callbacks)
        let s:jobs[job] = s:job_cid
        let s:result_handlers[job] = a:handlers
    elseif has('job') && has('channel')
        let l:opts = {}
        let l:opts.mode = 'nl'
        let l:opts.out_cb = {ch, data -> rtags#HandleResults(ch_info(ch).id, data, 'vim_stdout')}
        let l:opts.exit_cb = {ch, data -> rtags#HandleResults(ch_info(ch).id, data,'vim_exit')}
        let l:opts.stoponexit = 'kill'
        let job = job_start(cmd, l:opts)
        let channel = ch_info(job_getchannel(job)).id
        let s:result_stdout[channel] = []
        let s:jobs[channel] = s:job_cid
        let s:result_handlers[channel] = a:handlers
    endif

endfunction

function! rtags#HandleResults(job_id, data, event)


    if a:event == 'vim_stdout'
        call add(s:result_stdout[a:job_id], a:data)
    elseif a:event == 'vim_exit'

        let job_cid = remove(s:jobs, a:job_id)
        let handlers = remove(s:result_handlers, a:job_id)
        let output = remove(s:result_stdout, a:job_id)

        call rtags#ExecuteHandlers(output, handlers)
    else
        let job_cid = remove(s:jobs, a:job_id)
        let temp_file = rtags#TempFile(job_cid)
        let output = readfile(temp_file)
        let handlers = remove(s:result_handlers, a:job_id)
        call rtags#ExecuteHandlers(output, handlers)
        execute 'silent !rm -f ' . temp_file
    endif

endfunction

function! rtags#ExecuteHandlers(output, handlers)
    let result = a:output
    for Handler in a:handlers
        if type(Handler) == 3
            let HandlerFunc = Handler[0]
            let args = Handler[1]
            let result = HandlerFunc(result, args)
        else
            try
                let result = Handler(result)
            catch /E706/
                " If we're not returning the right type we're probably done
                return
            endtry
        endif
    endfor
    return result
endfunction

function! rtags#ExecuteNow(args, handlers)
    let l:result = rtags#ExecuteRC(a:args)
    if len(a:handlers) > 0
        return rtags#ExecuteHandlers(l:result, a:handlers)
    else
        return l:result
    endif
endfunction

function! rtags#ExecuteThen(args, handlers)
    if s:rtagsAsync == 1
        call rtags#ExecuteRCAsync(a:args, a:handlers)
    else
        call rtags#ExecuteNow(a:args, a:handlers)
    endif
endfunction

function! rtags#FindRefs()
    let args = {
                \ '-e' : '',
                \ '--containing-function-location' : '',
                \ '-r' : rtags#getCurrentLocation() }

    call rtags#ExecuteThen(args, [function('rtags#DisplayResults')])
endfunction

function! rtags#ShowHierarchy()
    let args = {'--class-hierarchy' : rtags#getCurrentLocation() }

    call rtags#ExecuteThen(args, [function('rtags#ViewHierarchy')])
endfunction

function! rtags#FindRefsCallTree()
    let args = {
                \ '--containing-function-location' : '',
                \ '-r' : rtags#getCurrentLocation() }

    call rtags#ExecuteThen(args, [function('rtags#ViewReferences')])
endfunction

function! rtags#FindSuperClasses()
    call rtags#ExecuteThen({ '--class-hierarchy' : rtags#getCurrentLocation() },
                \ [function('rtags#ExtractSuperClasses'), function('rtags#DisplayResults')])
endfunction

function! rtags#FindSubClasses()
    let result = rtags#ExecuteThen({ '--class-hierarchy' : rtags#getCurrentLocation() }, [
                \ function('rtags#ExtractSubClasses'),
                \ function('rtags#DisplayResults')])
endfunction

function! rtags#FindVirtuals()
    let args = {
                \ '-k' : '',
                \ '-r' : rtags#getCurrentLocation() }

    call rtags#ExecuteThen(args, [function('rtags#DisplayResults')])
endfunction

function! rtags#FindRefsByName(name)
    let args = {
                \ '-a' : '',
                \ '-e' : '',
                \ '-R' : a:name }

    call rtags#ExecuteThen(args, [function('rtags#DisplayResults')])
endfunction

" case insensitive FindRefsByName
function! rtags#IFindRefsByName(name)
    let args = {
                \ '-a' : '',
                \ '-e' : '',
                \ '-R' : a:name,
                \ '-I' : '' }

    call rtags#ExecuteThen(args, [function('rtags#DisplayResults')])
endfunction

" Find all those references which has the name which is equal to the word
" under the cursor
function! rtags#FindRefsOfWordUnderCursor()
    let wordUnderCursor = expand("<cword>")
    call rtags#FindRefsByName(wordUnderCursor)
endfunction

""" rc -HF <pattern>
function! rtags#FindSymbols(pattern)
    let args = {
                \ '-a' : '',
                \ '-F' : a:pattern }

    call rtags#ExecuteThen(args, [function('rtags#DisplayResults')])
endfunction

function! rtags#CompleteSymbolsFilter(results)
    let l:extracted = []
    for l:line in a:results
        let l:pos = match(l:line, '(')
        if l:pos < 0
            call add(l:extracted, l:line)
        endif
    endfor
    " for l:line in l:extracted
    "   echomsg l:line
    " endfor
    return l:extracted
endfunction

" Method for tab-completion for vim's commands
function! rtags#CompleteSymbols(arg, line, pos)
    if len(a:arg) < g:rtagsMinCharsForCommandCompletion
        return []
    endif
    return rtags#ExecuteNow({ '-S' : a:arg }, [function('rtags#CompleteSymbolsFilter')])
endfunction

" case insensitive FindSymbol
function! rtags#IFindSymbols(pattern)
    let args = {
                \ '-a' : '',
                \ '-I' : '',
                \ '-F' : a:pattern }

    call rtags#ExecuteThen(args, [function('rtags#DisplayResults')])
endfunction

function! rtags#ProjectListHandler(output)
    let projects = a:output
    let i = 1
    for p in projects
        echo '['.i.'] '.p
        let i = i + 1
    endfor
    let choice = input('Choice: ')
    if choice > 0 && choice <= len(projects)
        call rtags#ProjectOpen(projects[choice-1])
    endif
endfunction

function! rtags#ProjectList()
    call rtags#ExecuteThen({ '-w' : '' }, [function('rtags#ProjectListHandler')])
endfunction

function! rtags#ProjectOpen(pattern)
    call rtags#ExecuteThen({ '-w' : a:pattern }, [])
endfunction

function! rtags#LoadCompilationDb(pattern)
    call rtags#ExecuteThen({ '-J' : a:pattern }, [])
endfunction

function! rtags#ProjectClose(pattern)
    call rtags#ExecuteThen({ '-u' : a:pattern }, [])
endfunction

function! rtags#PreprocessFileHandler(result)
    vnew
    call append(0, a:result)
endfunction

function! rtags#PreprocessFile()
    call rtags#ExecuteThen({ '-E' : expand("%:p") }, [function('rtags#PreprocessFileHandler')])
endfunction

function! rtags#ReindexFile()
    call rtags#ExecuteThen({ '-V' : expand("%:p") }, [])
endfunction

function! rtags#FindSymbolsOfWordUnderCursor()
    let wordUnderCursor = expand("<cword>")
    call rtags#FindSymbols(wordUnderCursor)
endfunction

function! rtags#Diagnostics()
    let s:file = expand("%:p")
    return s:Pyeval("vimrtags.get_diagnostics()")
endfunction

"
" This function assumes it is invoked from insert mode
"
function! rtags#CompleteAtCursor(wordStart, base)
    let flags = "--synchronous-completions -l"
    let file = expand("%:p")
    let pos = getpos('.')
    let line = pos[1]
    let col = pos[2]

    if index(['.', '::', '->'], a:base) != -1
        let col += 1
    endif

    let rcRealCmd = rtags#getRcCmd()

    exec "normal! \<Esc>"
    let stdin_lines = join(getline(1, "$"), "\n").a:base
    let offset = len(stdin_lines)

    exec "startinsert!"
    "    echomsg getline(line)
    "    sleep 1
    "    echomsg "DURING INVOCATION POS: ".pos[2]
    "    sleep 1
    "    echomsg stdin_lines
    "    sleep 1
    " sed command to remove CDATA prefix and closing xml tag from rtags output
    let sed_cmd = "sed -e 's/.*CDATA\\[//g' | sed -e 's/.*\\/completions.*//g'"
    let cmd = printf("%s %s %s:%s:%s --unsaved-file=%s:%s | %s", rcRealCmd, flags, file, line, col, file, offset, sed_cmd)
    call rtags#Log("Command line:".cmd)

    let result = split(system(cmd, stdin_lines), '\n\+')
    "    echomsg "Got ".len(result)." completions"
    "    sleep 1
    call rtags#Log("-----------")
    "call rtags#Log(result)
    call rtags#Log("-----------")
    return result
    "    for r in result
    "        echo r
    "    endfor
    "    call rtags#DisplayResults(result)
endfunction

function! s:Pyeval( eval_string )
  if g:rtagsPy == 'python3'
      return py3eval( a:eval_string )
  else
      return pyeval( a:eval_string )
  endif
endfunction

function! s:RcExecuteJobCompletion()
    call rtags#SetJobStateFinish()
    if ! empty(b:rtags_state['stdout']) && mode() == 'i'
        call feedkeys("\<C-x>\<C-o>", "t")
    else
        call RtagsCompleteFunc(0, RtagsCompleteFunc(1, 0))
    endif
endfunction

"{{{ RcExecuteJobHandler
"Handles stdout/stderr/exit events, and stores the stdout/stderr received from the shells.
function! RcExecuteJobHandler(job_id, data, event)
    if a:event == 'exit'
        call s:RcExecuteJobCompletion()
    else
        call rtags#AddJobStandard(a:event, a:data)
    endif
endf

function! rtags#SetJobStateFinish()
    let b:rtags_state['state'] = 'finish'
endfunction

function! rtags#AddJobStandard(eventType, data)
    call add(b:rtags_state[a:eventType], a:data)
endfunction

function! rtags#SetJobStateReady()
    let b:rtags_state['state'] = 'ready'
endfunction

function! rtags#IsJobStateReady()
    if b:rtags_state['state'] == 'ready'
        return 1
    endif
    return 0
endfunction

function! rtags#IsJobStateBusy()
    if b:rtags_state['state'] == 'busy'
        return 1
    endif
    return 0
endfunction

function! rtags#IsJobStateFinish()
    if b:rtags_state['state'] == 'finish'
        return 1
    endif
    return 0
endfunction


function! rtags#SetStartJobState()
    let b:rtags_state['state'] = 'busy'
    let b:rtags_state['stdout'] = []
    let b:rtags_state['stderr'] = []
endfunction

function! rtags#GetJobStdOutput()
    return b:rtags_state['stdout']
endfunction

function! rtags#ExistsAndCreateRtagsState()
    if !exists('b:rtags_state')
        let b:rtags_state = { 'state': 'ready', 'stdout': [], 'stderr': [] }
    endif
endfunction

"{{{ s:RcExecute
" Execute clang binary to generate completions and diagnostics.
" Global variable:
" Buffer vars:
"     b:rtags_state => {
"       'state' :  // updated to 'ready' in sync mode
"       'stdout':  // updated in sync mode
"       'stderr':  // updated in sync mode
"     }
"
"     b:clang_execute_job_id  // used to stop previous job
"
" @root Clang root, project directory
" @line Line to complete
" @col Column to complete
" @return [completion, diagnostics]
function! s:RcJobExecute(offset, line, col)

    let file = expand("%:p")
    let l:cmd = printf("rc --absolute-path --synchronous-completions -l %s:%s:%s --unsaved-file=%s:%s", file, a:line, a:col, file, a:offset)

    if exists('b:rc_execute_job_id') && job_status(b:rc_execute_job_id) == 'run'
      try
        call job_stop(b:rc_execute_job_id, 'term')
        unlet b:rc_execute_job_id
      catch
        " Ignore
      endtry
    endif

    call rtags#SetStartJobState()

    let l:argv = l:cmd
    let l:opts = {}
    let l:opts.mode = 'nl'
    let l:opts.in_io = 'buffer'
    let l:opts.in_buf = bufnr('%')
    let l:opts.out_cb = {ch, data -> RcExecuteJobHandler(ch, data,  'stdout')}
    let l:opts.err_cb = {ch, data -> RcExecuteJobHandler(ch, data,  'stderr')}
    let l:opts.exit_cb = {ch, data -> RcExecuteJobHandler(ch, data, 'exit')}
    let l:opts.stoponexit = 'kill'

    let l:jobid = job_start(l:argv, l:opts)
    let b:rc_execute_job_id = l:jobid

    if job_status(l:jobid) != 'run'
        unlet b:rc_execute_job_id
    endif

endf

"""
" Temporarily the way this function works is:
"     - completeion invoked on
"         object.meth*
"       , where * is cursor position
"     - find the position of a dot/arrow
"     - invoke completion through rc
"     - filter out options that start with meth (in this case).
"     - show completion options
"
"     Reason: rtags returns all options regardless of already type method name
"     portion
"""

function! RtagsCompleteFunc(findstart, base)
    if s:rtagsAsync == 1 && !has('nvim')
        return s:RtagsCompleteFunc(a:findstart, a:base, 1)
    else
        return s:RtagsCompleteFunc(a:findstart, a:base, 0)
    endif
endfunction

function! s:RtagsCompleteFunc(findstart, base, async)
    call rtags#Log("RtagsCompleteFunc: [".a:findstart."], [".a:base."]")

    if a:findstart
        let s:line = getline('.')
        let s:start = col('.') - 2
        return s:Pyeval("vimrtags.get_identifier_beginning()")
    else
        let pos = getpos('.')
        let s:file = expand("%:p")
        let s:line = str2nr(pos[1])
        let s:col = str2nr(pos[2]) + len(a:base)
        let s:prefix = a:base
        return s:Pyeval("vimrtags.send_completion_request()")
    endif
endfunction

if g:rtagsInsertModeCompletion == 1
    if &completefunc == ""
        set completefunc=RtagsCompleteFunc
    endif
endif

" Helpers to access script locals for unit testing {{{
function! s:get_SID()
    return matchstr(expand('<sfile>'), '<SNR>\d\+_')
endfunction
let s:SID = s:get_SID()
delfunction s:get_SID

function! rtags#__context__()
    return { 'sid': s:SID, 'scope': s: }
endfunction
"}}}

command! -nargs=1 -complete=customlist,rtags#CompleteSymbols RtagsFindSymbols call rtags#FindSymbols(<q-args>)
command! -nargs=1 -complete=customlist,rtags#CompleteSymbols RtagsFindRefsByName call rtags#FindRefsByName(<q-args>)

command! -nargs=1 -complete=customlist,rtags#CompleteSymbols RtagsIFindSymbols call rtags#IFindSymbols(<q-args>)
command! -nargs=1 -complete=customlist,rtags#CompleteSymbols RtagsIFindRefsByName call rtags#IFindRefsByName(<q-args>)

" command! -nargs=1 -complete=dir RtagsLoadCompilationDb call rtags#LoadCompilationDb(<q-args>)

" The most commonly used find operation
command! -nargs=1 -complete=customlist,rtags#CompleteSymbols Rtag RtagsIFindSymbols <args>

