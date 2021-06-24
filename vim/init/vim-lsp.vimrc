if executable('ccls')
    au User lsp_setup call lsp#register_server({
		\ 'name': 'ccls',
		\ 'cmd': {server_info->['ccls']},
		\ 'root_uri': {server_info->lsp#utils#path_to_uri(
		\   lsp#utils#find_nearest_parent_file_directory(
		\     lsp#utils#get_buffer_path(), ['.ccls', 'compile_commands.json', '.git/']))},
		\ 'initialization_options': {
		\   'highlight': { 'lsRanges' : v:true },
		\ },
		\ 'whitelist': ['c', 'cpp', 'objc', 'objcpp', 'cc'],
		\ })
endif

" LSP

function! s:on_lsp_buffer_enabled() abort
    setlocal omnifunc=lsp#complete
    setlocal signcolumn=yes
    if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
    nmap <buffer> <leader>cd <plug>(lsp-definition)
    nmap <buffer> <leader>cD <plug>(lsp-references)
    nmap <buffer> <leader>cr <plug>(lsp-rename)

    let g:lsp_format_sync_timeout = 1000
    autocmd! BufWritePre *.rs,*.go call execute('LspDocumentFormatSync')
endfunction

augroup lsp_install
    au!
    " call s:on_lsp_buffer_enabled only for languages that has the server registered.
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END
