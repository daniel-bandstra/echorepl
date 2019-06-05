(require 'echorepl)
(in-package :echorepl)

(start-recording)
(stop-recording)
(reset-score)

(loop-button)
(undo-button)
(connect-input 1)
(connect-input 0)

(length *score*)
(asdf:load-system 'echorepl :force t)

(wait-for-score-change)
