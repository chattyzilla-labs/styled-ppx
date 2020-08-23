open Jest;

let supportList = [
  [%styled.inline "overflow-x: clip"],
  [%styled.inline "opacity: 0.9"],
  [%styled.inline "box-shadow: 1px 54px 1px blue"],
  [%styled.inline "box-shadow: 2px 3px blue"],
  [%styled.inline "text-shadow: 0px 0px blue"],
  [%styled.inline "text-shadow: 10px 0px 0px blue"],
  [%styled.inline {| overflow-y: visible; overflow-x: visible; overflow: hidden; |}],
  [%styled.inline "visibility: visible"],
  [%styled.inline "hyphens: manual"],
  //  [%styled.inline "stroke: none"],
  [%styled.inline "order: 0"],
  [%styled.inline "direction: ltr"],
  //  [%styled.inline "content: normal"],
  [%styled.inline "clear: none"],
  [%styled.inline "box-sizing: content-box"],
  [%styled.inline "box-sizing: border-box"],
  // [%styled.inline "box-shadow: none"],
  // [%styled.inline "border-collapse: separate"],
  [%styled.inline "transition-property: all"],
  [%styled.inline "transition-duration: 0.5s"],
  [%styled.inline "transition-timing-function: ease"],
  [%styled.inline "transition-timing-function: step-end"],
  [%styled.inline "transition-delay: 0.5s"],
  [%styled.inline "transition: none;"],
  [%styled.inline "transition: ease 250ms"],
  [%styled.inline "transition: ease 250ms"],
  [%styled.inline "transition: margin-left 4s ease-in-out 1s"],
  [%styled.inline "transition: width 2s, height 2s, background-color 2s, transform 2s"],
  [%styled.inline "animation-name: slidein"],
  [%styled.inline "animation-duration: 3s"],
  [%styled.inline "animation-timing-function: ease"],
  [%styled.inline "animation-delay: 3s"],
  [%styled.inline "animation-direction: alternate"],
  [%styled.inline "animation-iteration-count: infinite"],
  [%styled.inline "animation-iteration-count: 1"],
  [%styled.inline "animation-iteration-count: 2, 1, 5"],
  [%styled.inline "animation-fill-mode: backwards"],
  //  [%styled.inline "animation-play-state: "],
  //  [%styled.inline "animation: 3s infinite alternate slidein"],
  [%styled.inline "transform: translate(10px, 10px)"],
  [%styled.inline "transform: translateX(10px) rotate(10deg) translateY(5px)"],
  [%styled.inline "transform: matrix(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)"],
  [%styled.inline "transform: translate(12px, 50%)"],
  [%styled.inline "transform: translateX(2em)"],
  [%styled.inline "transform: translateY(3in)"],
  [%styled.inline "transform: scale(2, 0.5)"],
  [%styled.inline "transform: scaleX(2)"],
  [%styled.inline "transform: scaleY(0.5)"],
  [%styled.inline "transform: rotate(0.5turn)"],
  [%styled.inline "transform: skew(30deg, 20deg)"],
  [%styled.inline "transform: skewX(30deg)"],
  [%styled.inline "transform: skewY(1.07rad)"],
  [%styled.inline
    "transform: matrix3d(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0)"
  ],
  [%styled.inline "transform: translate3d(12px, 50%, 3em)"],
  [%styled.inline "transform: translateZ(2px)"],
  [%styled.inline "transform: scale3d(2.5, 1.2, 0.3)"],
  [%styled.inline "transform: scaleZ(0.3)"],
  [%styled.inline "transform: rotate3d(1, 2.0, 3.0, 10deg)"],
  [%styled.inline "transform: rotateX(10deg)"],
  [%styled.inline "transform: rotateY(10deg)"],
  [%styled.inline "transform: rotateZ(10deg)"],
  [%styled.inline "transform: perspective(17px)"],
  [%styled.inline "font-family: 'Open Sans', '-system', sans-serif"],
  [%styled.inline "transform: initial"],
  [%styled.inline "flex-flow: row wrap"],
  [%styled.inline "flex: 1 2 content"],
  [%styled.inline "flex: unset"],
  // css-sizing-3
  [%styled.inline "width: auto"],
  [%styled.inline "width: 0"],
  [%styled.inline "height: 5px"],
  [%styled.inline "min-width: 5%"],
  [%styled.inline "min-height: 5em"],
  [%styled.inline "max-width: none"],
  [%styled.inline "max-height: 3vh"],
  [%styled.inline "box-sizing: border-box"],
  // css-box-3
  [%styled.inline "margin-top: auto"],
  [%styled.inline "margin-right: 1px"],
  [%styled.inline "margin-bottom: 2px"],
  [%styled.inline "margin-left: 3px"],
  [%styled.inline "margin: 1px"],
  [%styled.inline "margin: 1px 2px"],
  [%styled.inline "margin: 1px 2px 3px"],
  [%styled.inline "margin: 1px 2px 3px 4px"],
  [%styled.inline "padding-top: 0"],
  [%styled.inline "padding-right: 1px"],
  [%styled.inline "padding-bottom: 2px"],
  [%styled.inline "padding-left: 3px"],
  [%styled.inline "padding: 1px"],
  [%styled.inline "padding: 1px 2px"],
  [%styled.inline "padding: 1px 2px 3px"],
  [%styled.inline "padding: 1px 2px 3px 4px"],
  // css-color-4
  [%styled.inline "color: #012"],
  [%styled.inline "color: #0123"],
  [%styled.inline "color: #012345"],
  [%styled.inline "color: #01234567"],
  [%styled.inline "color: blue"],
  [%styled.inline "color: currentcolor"],
  [%styled.inline "color: transparent"],
  [%styled.inline "color: rgb(1 2 3)"],
  [%styled.inline "color: rgb(1 2 3 / .4)"],
  [%styled.inline "color: rgba(1, 2, 3)"],
  [%styled.inline "color: rgba(1, 2, 3, .4)"],
  [%styled.inline "color: hsl(120deg 100% 50%)"],
  [%styled.inline "opacity: 0.5"],
  [%styled.inline "opacity: 60%"],
  // css-images-4
  [%styled.inline "object-fit: fill"],
  [%styled.inline "object-position: right bottom"],
  // css-backgrounds-3
  [%styled.inline "background-color: red"],
  [%styled.inline "border-top-color: blue"],
  [%styled.inline "border-right-color: green"],
  [%styled.inline "border-bottom-color: purple"],
  [%styled.inline "border-left-color: #fff"],
  [%styled.inline "border-top-width: 15px"],
  [%styled.inline "border-right-width: 16px"],
  [%styled.inline "border-bottom-width: 17px"],
  [%styled.inline "border-left-width: 18px"],
  [%styled.inline "border-top-left-radius: 12%"],
  [%styled.inline "border-top-right-radius: 15%"],
  [%styled.inline "border-bottom-left-radius: 14%"],
  [%styled.inline "border-bottom-right-radius: 13%"],
  [%styled.inline "box-shadow: 12px 12px 2px 1px rgba(0, 0, 255, .2)"],
  [%styled.inline
    "box-shadow: 12px 12px 2px 1px rgba(0, 0, 255, .2), 13px 14px 5px 6px rgba(2, 1, 255, 50%)"
  ],
  // css-overflow-3
  [%styled.inline "overflow-x: auto"],
  [%styled.inline "overflow-y: hidden"],
  [%styled.inline "overflow: scroll"],
  [%styled.inline "overflow: scroll visible"],
  [%styled.inline "text-overflow: clip"],
  [%styled.inline "text-overflow: ellipsis"],
  // css-text-3
  [%styled.inline "text-transform: capitalize"],
  [%styled.inline "white-space: break-spaces"],
  [%styled.inline "word-break: keep-all"],
  [%styled.inline "overflow-wrap: anywhere"],
  [%styled.inline "word-wrap: normal"],
  // [%styled.inline "text-align: start"],
  [%styled.inline "text-align: left"],
  [%styled.inline "word-spacing: normal"],
  [%styled.inline "word-spacing: 5px"],
  [%styled.inline "letter-spacing: normal"],
  [%styled.inline "letter-spacing: 5px"],
  [%styled.inline "text-indent: 5%"],
  // not supported
  [%styled.inline "-moz-text-blink: blink"],
  [%styled.inline "display: -webkit-inline-box"],
  // media-query
  [%styled.inline "@media (min-width: 30em) and (min-height: 20em) { color: brown; }"],
];

Belt.List.forEachWithIndex(supportList, (index, css) => {
  test("Component " ++ string_of_int(index) ++ " renders", () => {
    css |> Expect.expect |> Expect.toMatchSnapshot
  })
});
