# Notice
```
I'm not good at English.
So please correct my English.
```
Flex Autopair automatically insert pair braces and quotes, insertion conditions & actions are highly customizable.

# Features
- Less configuration: Auto detect pairs from syntax table.
- More customizable: Insertion conditions & actions are highly customizable.

# Default setting
- Auto insert matching pair(e.g. quote bracket paren)
- Undo can cancel only auto insertion
- Skip close pair when you press by mistake
- Wrap something(e.g. word symbol url) with pairs
In c like languages and lisp languages have additional behavior. It is explained in demo video.

# Demo video

<iframe src="http://player.vimeo.com/video/39530265" width="500" height="375" frameborder="0" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe>

# Setup
You can install from github.

```lisp
(auto-install-from-url "https://raw.github.com/uk-ar/flex-autopair/master/flex-autopair.el")
```

Or you can install from marmalade.

```
M-x package-install flex-autopair
```

Then modify your .emacs like this.

```lisp
(require 'flex-autopair)
(flex-autopair-mode 1)
```

# Customizing action in specific condition
You can highly customize Insertion conditions & actions in this elisp. These behavior can be described by flex-autopair-conditions & flex-autopair-actions variables.

## flex-autopair-conditions
flex-autopair-conditions means conditions when inserting action should execute. This variable is a association list of conditional expressions vs corresponding action names(symbol). An ordinary element of the alist looks like (sexp . action-name)

Here is the example code which is a part of default flex-autopair-conditions.

```lisp
(setq flex-autopair-conditions
      `(;; Insert matching pair.
        (openp . pair)
        ;; Skip self.
        ((and closep
              (eq (char-after) last-command-event)) . skip)
        (closep . self)
        ))
```

flex-autopair.el searches flex-autopair-conditions for the first element in which conditional expression is true, then execute action of the element.
In the setting show above behave like this

1. Executing "pair" when you press open pair (open bracket, open paren, or open quote).
2. Executing "skip" when you press close pair and the charactor at point is same as you press.
3. Executing "self" when you press close pair and the other conditions.

openp is a variable which is set to t when you press open pair. closep is the opposite of openp: it is set to t if when you press close pair.

Those actions(pair, skip, and self) are mapped to actual behavior by flex-autopair-actions.

## flex-autopair-actions
This variable is a association list of action names(symbol) vs corresponding behavior(any S-expressions).An ordinary element of the alist looks like (action-name . sexp)

Here is the example code which is a part of default flex-autopair-actions.

```lisp
(setq flex-autopair-actions
      '((pair . (progn (call-interactively 'self-insert-command)
                       (save-excursion
                         (insert closer))))
        (skip . (forward-char 1))
        (self . (call-interactively 'self-insert-command))
        ))
```

flex-autopair.el looks up action name in flex-autopair-actions, and evaluates its associated S expression.
In the setting show above behave like this
+ pair: Insert matching pair.
+ skip: Only move forward one character.
+ self: inserts the last character typed.
You can add your setting in flex-autopair-conditions.

```lisp
(setq flex-autopair-user-conditions-high
      `((openp . hoge)
        (closep . fuga)))
(flex-autopair-reload-conditions)
```

flex-autopair-reload-conditions applies flex-autopair-user-conditions-high to flex-autopair-conditions with proper priority.
And you can add your setting in flex-autopair-conditions.

```lisp
(add-to-list
 'flex-autopair-actions
 '(hoge . (message "this is hoge"))
 )
```

# Adding pair charactor.
Flex-autopair automatically detect pairs from syntax table. But There are some situations in which you sometime want to insert pair, and others insert only you pressed. For example, "<" should be treated as pair only after "#include" directive in C language.

You can do this with the following steps.

1. Adding new pair of charactor
2. Adding condition of the pair.
3. Adding new pair of charactor

flex-autopair-pairs is a association list of open pair charactor vs close pair charactor. An ordinary element of the alist looks like (?open-pair . ?close-pair). flex-autopair-pairs is   buffer local variable so that you can set pairs each major mode.

Here is the example code which enables "<" and ">" pair in c-mode.

```lisp
(defun my-hook-function ()
  (add-to-list 'flex-autopair-pairs '(?\< . ?\>)))
(add-hook 'c-mode-hook 'my-hook-function)
```

# Adding conditions for the pair
Flex-autopair insert matching pair when you press open pair in the default setting. So you should add 2 settings for "<" in c-mode.

1. Executing "pair" when "#include" directive exist.
2. Executing "self" when "#include" directive is not exist.
Sample code is like this.

```lisp
(setq flex-autopair-user-conditions-high
      `(((and
          (eq major-mode 'c-mode)
          (eq last-command-event ?<)
          (save-excursion (re-search-backward "#include" (point-at-bol) t)))
         . pair)
        ((and
          (eq major-mode 'c-mode)
          (eq last-command-event ?<))
         . self)
        ))
(flex-autopair-reload-conditions)
```

That's all.
