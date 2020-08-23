open Migrate_parsetree;
open Ast_410;
open Setup;

let extract_tests = array_expr => {
  open Ppxlib.Ast_pattern;
  let fail = () =>
    failwith("can only extract from array((result, expected))");
  let payload = pexp_array(many(pexp_tuple(many(__))));
  parse(
    payload,
    Location.none,
    ~on_error=fail,
    array_expr,
    List.map(
      fun
      | [result, expected] => (result, expected)
      | _ => fail(),
    ),
  );
};
let write_tests_to_file = (tests, file) => {
  let code =
    tests
    |> List.map(((expected, _)) => [%stri let _ = [%e expected]])
    |> List.append([[%stri open StyledPpxTestNativeBSCSS]])
    |> Pprintast.string_of_structure;
  let fd = open_out(file);
  output_string(fd, code);
  close_out(fd);
};

let compare = (result, expected, {expect, _}) => {
  open Parsetree;
  let result =
    switch (result) {
    | {pexp_desc: Pexp_apply(_, [(_, expr)]), _} => expr
    | _ => failwith("probably the result changed")
    };

  let result = Pprintast.string_of_expression(result);
  let expected = Pprintast.string_of_expression(expected);
  expect.string(result).toEqual(expected);
};

// TODO: ideas, selectors . properties, to have a bigger test matrix
// somehow programatically generate strings to test css
let properties_static_css_tests = [%expr
  [|
    // unsupported
    ([%styled.inline "overflow-x: clip"], [Css.unsafe("overflowX", "clip")]),
    // ([%styled.inline "align-items: center"], [Css.alignItems(`center)]),
    ([%styled.inline "box-sizing: border-box"], [Css.boxSizing(`borderBox)]),
    ([%styled.inline "box-sizing: content-box"], [Css.boxSizing(`contentBox)]),
    ([%styled.inline "color: #454545"], [Css.color(`hex("454545"))]),
    ([%styled.inline "color: red"], [Css.color(Css.red)]),
    ([%styled.inline "display: flex"], [Css.unsafe("display", "flex")]),
    ([%styled.inline "flex-direction: column"], [Css.flexDirection(`column)]),
    ([%styled.inline "font-size: 30px"], [Css.unsafe("fontSize", "30px")]),
    ([%styled.inline "height: 100vh"], [Css.height(`vh(100.))]),
    // (
    //   [%styled.inline "justify-content: center"],
    //   [Css.unsafe("justifyContent", "center")],
    // ),
    ([%styled.inline "margin: 0"], [Css.margin(`zero)]),
    ([%styled.inline "margin: 5px"], [Css.margin(`pxFloat(5.))]),
    ([%styled.inline "opacity: 0.9"], [Css.opacity(0.9)]),
    ([%styled.inline "width: 100vw"], [Css.width(`vw(100.))]),
    // css-sizing-3
    ([%styled.inline "width: auto"], [Css.width(`auto)]),
    ([%styled.inline "width: 0"], [Css.width(`zero)]),
    ([%styled.inline "height: 5px"], [Css.height(`pxFloat(5.))]),
    ([%styled.inline "min-width: 5%"], [Css.minWidth(`percent(5.))]),
    ([%styled.inline "min-height: 5em"], [Css.minHeight(`em(5.))]),
    ([%styled.inline "max-width: 3em"], [Css.maxWidth(`em(3.))]),
    ([%styled.inline "max-height: 3vh"], [Css.maxHeight(`vh(3.))]),
    ([%styled.inline "box-sizing: border-box"], [Css.boxSizing(`borderBox)]),
    // css-box-3
    ([%styled.inline "margin-top: auto"], [Css.marginTop(`auto)]),
    ([%styled.inline "margin-right: 1px"], [Css.marginRight(`pxFloat(1.))]),
    ([%styled.inline "margin-bottom: 2px"], [Css.marginBottom(`pxFloat(2.))]),
    ([%styled.inline "margin-left: 3px"], [Css.marginLeft(`pxFloat(3.))]),
    ([%styled.inline "margin: 1px"], [Css.margin(`pxFloat(1.))]),
    (
      [%styled.inline "margin: 1px 2px"],
      [Css.margin2(~v=`pxFloat(1.), ~h=`pxFloat(2.))],
    ),
    (
      [%styled.inline "margin: 1px 2px 3px"],
      [
        Css.margin3(
          ~top=`pxFloat(1.),
          ~h=`pxFloat(2.),
          ~bottom=`pxFloat(3.),
        ),
      ],
    ),
    (
      [%styled.inline "margin: 1px 2px 3px 4px"],
      [
        Css.margin4(
          ~top=`pxFloat(1.),
          ~right=`pxFloat(2.),
          ~bottom=`pxFloat(3.),
          ~left=`pxFloat(4.),
        ),
      ],
    ),
    ([%styled.inline "padding-top: 0"], [Css.paddingTop(`zero)]),
    ([%styled.inline "padding-right: 1px"], [Css.paddingRight(`pxFloat(1.))]),
    ([%styled.inline "padding-bottom: 2px"], [Css.paddingBottom(`pxFloat(2.))]),
    ([%styled.inline "padding-left: 3px"], [Css.paddingLeft(`pxFloat(3.))]),
    ([%styled.inline "padding: 1px"], [Css.padding(`pxFloat(1.))]),
    (
      [%styled.inline "padding: 1px 2px"],
      [Css.padding2(~v=`pxFloat(1.), ~h=`pxFloat(2.))],
    ),
    (
      [%styled.inline "padding: 1px 2px 3px"],
      [
        Css.padding3(
          ~top=`pxFloat(1.),
          ~h=`pxFloat(2.),
          ~bottom=`pxFloat(3.),
        ),
      ],
    ),
    (
      [%styled.inline "padding: 1px 2px 3px 4px"],
      [
        Css.padding4(
          ~top=`pxFloat(1.),
          ~right=`pxFloat(2.),
          ~bottom=`pxFloat(3.),
          ~left=`pxFloat(4.),
        ),
      ],
    ),
    ([%styled.inline "color: #012"], [Css.color(`hex("012"))]),
    ([%styled.inline "color: #0123"], [Css.color(`hex("0123"))]),
    ([%styled.inline "color: #012345"], [Css.color(`hex("012345"))]),
    ([%styled.inline "color: #01234567"], [Css.color(`hex("01234567"))]),
    ([%styled.inline "color: blue"], [Css.color(Css.blue)]),
    ([%styled.inline "color: currentcolor"], [Css.color(`currentColor)]),
    ([%styled.inline "color: transparent"], [Css.color(`transparent)]),
    ([%styled.inline "color: rgb(1 2 3)"], [Css.color(`rgb((1, 2, 3)))]),
    ([%styled.inline "color: rgb(1 2 3 / .4)"], [Css.color(`rgba((1, 2, 3, 0.4)))]),
    ([%styled.inline "color: rgba(1, 2, 3)"], [Css.color(`rgb((1, 2, 3)))]),
    (
      [%styled.inline "color: rgba(1, 2, 3, .4)"],
      [Css.color(`rgba((1, 2, 3, 0.4)))],
    ),
    (
      [%styled.inline "color: hsl(120deg 100% 50%)"],
      [Css.color(`hsl((`deg(120.), `percent(100.), `percent(50.))))],
    ),
    ([%styled.inline "opacity: 0.5"], [Css.opacity(0.5)]),
    ([%styled.inline "opacity: 60%"], [Css.opacity(0.6)]),
    // css-images-4
    ([%styled.inline "object-fit: fill"], [Css.objectFit(`fill)]),
    (
      [%styled.inline "object-position: right bottom"],
      [Css.objectPosition(`hv((`right, `bottom)))],
    ),
    // css-backgrounds-3
    ([%styled.inline "background-color: red"], [Css.backgroundColor(Css.red)]),
    ([%styled.inline "border-top-color: blue"], [Css.borderTopColor(Css.blue)]),
    ([%styled.inline "border-right-color: green"], [Css.borderRightColor(Css.green)]),
    (
      [%styled.inline "border-bottom-color: purple"],
      [Css.borderBottomColor(Css.purple)],
    ),
    ([%styled.inline "border-left-color: #fff"], [Css.borderLeftColor(`hex("fff"))]),
    ([%styled.inline "border-top-width: 15px"], [Css.borderTopWidth(`pxFloat(15.))]),
    (
      [%styled.inline "border-right-width: 16px"],
      [Css.borderRightWidth(`pxFloat(16.))],
    ),
    (
      [%styled.inline "border-bottom-width: 17px"],
      [Css.borderBottomWidth(`pxFloat(17.))],
    ),
    (
      [%styled.inline "border-left-width: 18px"],
      [Css.borderLeftWidth(`pxFloat(18.))],
    ),
    (
      [%styled.inline "border-top-left-radius: 12%"],
      [Css.borderTopLeftRadius(`percent(12.))],
    ),
    (
      [%styled.inline "border-top-right-radius: 15%"],
      [Css.borderTopRightRadius(`percent(15.))],
    ),
    (
      [%styled.inline "border-bottom-left-radius: 14%"],
      [Css.borderBottomLeftRadius(`percent(14.))],
    ),
    (
      [%styled.inline "border-bottom-right-radius: 13%"],
      [Css.borderBottomRightRadius(`percent(13.))],
    ),
    (
      [%styled.inline "box-shadow: 12px 12px 2px 1px rgba(0, 0, 255, .2)"],
      [
        Css.boxShadows([
          Css.Shadow.box(
            ~x=`pxFloat(12.),
            ~y=`pxFloat(12.),
            ~blur=`pxFloat(2.),
            ~spread=`pxFloat(1.),
            `rgba((0, 0, 255, 0.2)),
          ),
        ]),
      ],
    ),
    (
      [%styled.inline
        "box-shadow: 12px 12px 2px 1px rgba(0, 0, 255, .2), 13px 14px 5px 6px rgba(2, 1, 255, 50%)"
      ],
      [
        Css.boxShadows([
          Css.Shadow.box(
            ~x=`pxFloat(12.),
            ~y=`pxFloat(12.),
            ~blur=`pxFloat(2.),
            ~spread=`pxFloat(1.),
            `rgba((0, 0, 255, 0.2)),
          ),
          Css.Shadow.box(
            ~x=`pxFloat(13.),
            ~y=`pxFloat(14.),
            ~blur=`pxFloat(5.),
            ~spread=`pxFloat(6.),
            `rgba((2, 1, 255, 0.5)),
          ),
        ]),
      ],
    ),
    // css-overflow-3
    ([%styled.inline "overflow-x: auto"], [Css.overflowX(`auto)]),
    ([%styled.inline "overflow-y: hidden"], [Css.overflowY(`hidden)]),
    ([%styled.inline "overflow: scroll"], [Css.overflow(`scroll)]),
    (
      [%styled.inline "overflow: scroll visible"],
      [Css.overflowX(`scroll), Css.overflowY(`visible)],
    ),
    // ([%styled.inline "text-overflow: clip"], [Css.textOverflow(`clip)]),
    // ([%styled.inline "text-overflow: ellipsis"], [Css.textOverflow(`ellipsis)]),
    // css-text-3
    ([%styled.inline "text-transform: capitalize"], [Css.textTransform(`capitalize)]),
    ([%styled.inline "white-space: break-spaces"], [Css.whiteSpace(`breakSpaces)]),
    ([%styled.inline "word-break: keep-all"], [Css.wordBreak(`keepAll)]),
    ([%styled.inline "overflow-wrap: anywhere"], [Css.overflowWrap(`anywhere)]),
    ([%styled.inline "word-wrap: normal"], [Css.wordWrap(`normal)]),
    // ([%styled.inline "text-align: start"], [Css.textAlign(`start)]),
    ([%styled.inline "text-align: left"], [Css.textAlign(`left)]),
    ([%styled.inline "word-spacing: normal"], [Css.wordSpacing(`normal)]),
    ([%styled.inline "word-spacing: 5px"], [Css.wordSpacing(`pxFloat(5.))]),
    ([%styled.inline "letter-spacing: normal"], [Css.letterSpacing(`normal)]),
    ([%styled.inline "letter-spacing: 5px"], [Css.letterSpacing(`pxFloat(5.))]),
    ([%styled.inline "text-indent: 5%"], [Css.textIndent(`percent(5.))]),
    // css-flexbox-1
    ([%styled.inline "flex-wrap: wrap"], [Css.flexWrap(`wrap)]),
    // TODO: generate tests with variables in the future
    // ([%styled.inline "flex-wrap: $var"], [Css.flexWrap(var)]),
    // ([%styled.inline "flex-wrap: $(var)"], [Css.flexWrap(var)]),
    (
      [%styled.inline "flex-flow: row nowrap"],
      [Css.flexDirection(`row), Css.flexWrap(`nowrap)],
    ),
    // TODO: flex-flow + variables
    ([%styled.inline "order: 5"], [Css.order(5)]),
    ([%styled.inline "flex-grow: 2"], [Css.flexGrow(2.)]),
    ([%styled.inline "flex-grow: 2.5"], [Css.flexGrow(2.5)]),
    ([%styled.inline "flex-shrink: 2"], [Css.flexShrink(2.)]),
    ([%styled.inline "flex-shrink: 2.5"], [Css.flexShrink(2.5)]),
    ([%styled.inline "flex-basis: content"], [Css.flexBasis(`content)]),
    ([%styled.inline "flex: none"], [Css.flex(`none)]),
    (
      [%styled.inline "flex: 1 2 content"],
      [Css.flexGrow(1.), Css.flexShrink(2.), Css.flexBasis(`content)],
    ),
    // ([%styled.inline "align-self: stretch"], [Css.alignSelf(`stretch)]),
    // (
    //   [%styled.inline "align-content: space-around"],
    //   [Css.alignContent(`spaceAround)],
    // ),
    // not supported
    (
      [%styled.inline "-moz-text-blink: blink"],
      [Css.unsafe("MozTextBlink", "blink")],
    ),
    (
      [%styled.inline "display: -webkit-inline-box"],
      [Css.unsafe("display", "-webkit-inline-box")],
    ),
  |]
];
let selectors_static_css_tests = [%expr
  [|
    (
      [%styled.inline "& > a { color: green; }"],
      [Css.selector({js|& > a|js}, [Css.color(Css.green)])],
    ),
    (
      [%styled.inline "&:nth-child(even) { color: red; }"],
      [Css.selector({js|&:nth-child(even)|js}, [Css.color(Css.red)])],
    ),
    (
      [%styled.inline "& > div:nth-child(3n+1) { color: blue; }"],
      [
        Css.selector(
          {js|& > div:nth-child(3n  + 1)|js},
          [Css.color(Css.blue)],
        ),
      ],
    ),
    (
      [%styled.inline "&::active { color: brown; }"],
      [Css.active([Css.color(Css.brown)])],
    ),
    (
      [%styled.inline "&:hover { color: gray; }"],
      [Css.hover([Css.color(Css.gray)])],
    ),
  |]
];
let media_query_static_css_tests = [%expr
  [|
    (
      [%styled.inline {|color: blue; @media (min-width: 30em) { color: red; }|}],
      [
        Css.color(Css.blue),
        Css.media("(min-width: 30em)", [Css.color(Css.red)]),
      ],
    ),
    (
      [%styled.inline
        {|@media (min-width: 30em) and (min-height: 20em) { color: brown; }|}
      ],
      [
        Css.media(
          "(min-width: 30em) and (min-height: 20em)",
          [Css.color(Css.brown)],
        ),
      ],
    ),
  |]
];
describe("emit bs-css from static [%styled.inline]", ({test, _}) => {
  let test = (prefix, index, (result, expected)) =>
    test(prefix ++ string_of_int(index), compare(result, expected));
  let properties_static_css_tests =
    extract_tests(properties_static_css_tests);
  let selectors_static_css_tests = extract_tests(selectors_static_css_tests);
  let media_query_static_css_tests =
    extract_tests(media_query_static_css_tests);

  write_tests_to_file(properties_static_css_tests, "static_css_tests.ml");
  write_tests_to_file(selectors_static_css_tests, "selectors_css_tests.ml");
  write_tests_to_file(
    media_query_static_css_tests,
    "media_query_css_tests.ml",
  );

  List.iteri(test("properties static: "), properties_static_css_tests);
  List.iteri(test("selectors static: "), selectors_static_css_tests);
  List.iteri(test("media query static: "), media_query_static_css_tests);
});

let properties_variable_css_tests = [
  ([%expr [%styled.inline "color: $var"]], [%expr [Css.color(var)]]),
  // TODO: ([%styled.inline "margin: $var"], [%expr [Css.margin("margin", var)]),
];
describe("emit bs-css from variable [%styled.inline]", ({test, _}) => {
  let test = (index, (result, expected)) =>
    test(
      "simple variable: " ++ string_of_int(index),
      compare(result, expected),
    );
  List.iteri(test, properties_variable_css_tests);
});
