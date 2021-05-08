"if exists("g:blade_loaded")
"	finish
"endif
"let g:blade_loaded = 1

"function! s:fmt_autosave()
"	if get(g:, "blade_fmt_autosave", 1)
"		call blade#fmt#Format()
"	endif
"endfunction

"augroup vim-blade
"	autocmd!
"	autocmd BufWritePre *.bld call s:fmt_autosave()
"augroup end
