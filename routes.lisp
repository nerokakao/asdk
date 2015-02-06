(in-package :asdk)


(push
 (tbnl:create-prefix-dispatcher "/test" 'test)
 *dispatch-tables*)
