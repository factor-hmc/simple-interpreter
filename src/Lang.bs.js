// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");

function print_literal(literal) {
  if (typeof literal === "number") {
    if (literal === /* True */0) {
      console.log("True");
      return /* () */0;
    } else {
      console.log("False");
      return /* () */0;
    }
  } else {
    switch (literal.tag | 0) {
      case /* Int */0 :
          console.log("Int(" + (String(literal[0]) + ")"));
          return /* () */0;
      case /* String */1 :
          console.log("String");
          return /* () */0;
      case /* List */2 :
          console.log("List");
          return /* () */0;
      case /* Quotation */3 :
          console.log("Quotation");
          return /* () */0;
      
    }
  }
}

function print_stack(stack) {
  return List.iter(print_literal, stack);
}

function to_bool(literal) {
  if (typeof literal === "number") {
    return literal === 0;
  } else {
    return true;
  }
}

exports.print_literal = print_literal;
exports.print_stack = print_stack;
exports.to_bool = to_bool;
/* No side effect */
