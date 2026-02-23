Do NOT strip `-` from LANGUAGE pragma closings. The correct form is `#-}`, NOT `#}`.
Do NOT break lambda syntax or string literals.
Do NOT add unnecessary LANGUAGE extensions.
Run `sed -i 's/[[:space:]]*$//' <file>` after editing to remove trailing whitespace.
