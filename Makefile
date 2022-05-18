##
# Jake Shilling - Dotfiles
#
# @file
# @version 0.1

site:
	emacs -Q --batch -l ./publish.el --funcall dw/publish

clean-site:
	rm -rf public

# end
