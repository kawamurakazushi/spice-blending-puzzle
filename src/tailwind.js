const colors = {
  transparent: "transparent",
  primary: "rgba(0, 183, 0, 1)",
  primary55: "rgba(0, 183, 0, .55)",
  primary10: "rgba(0, 183, 0, .10)",
  error: "rgba(255, 0, 80, 1)",
  warning: "rgba(253, 190, 0, 1)",
  success: "rgba(0, 183, 0, 1)",
  black: "rgba(2, 8, 2, 1)",
  black90: "rgba(2, 8, 2, .90)",
  black55: "rgba(2, 8, 2, .55)",
  black10: "rgba(2, 8, 2, .10)",
  "border-grey": "rgba(239, 245, 241, 1)",
  "table-grey": "rgba(248, 250, 248, 1)",
  "background-grey": "rgba(245, 247, 245, 1)",
  "table-white": "rgba(250, 250, 250, 1)",
  white: "rgba(255, 255, 255, 1)",
  white55: "rgba(255, 255, 255, 0.55)"
};

const spacings = {
  auto: "auto",
  s0: 0,
  s1: 4,
  s2: 8,
  s3: 16,
  s4: 24,
  s5: 32,
  s6: 40,
  s7: 48,
  s8: 56,
  s9: 64,
  s10: 72,
  s11: 80,
  s12: 88,
  s13: 96,
  s14: 108
};

const spacingsWithPx = {
  auto: "auto",
  "0": "0px",
  "1": "4px",
  "2": "8px",
  "3": "16px",
  "4": "24px",
  "5": "32px",
  "6": "40px",
  "7": "48px",
  "8": "56px",
  "9": "64px",
  "10": "72px",
  "11": "80px",
  "12": "88px",
  "13": "96px",
  "14": "108px"
};

module.exports = {
  colors: colors,
  spacings: spacings,
  fonts: {
    sans: ["'M PLUS 1p'", "system-ui", "Ubuntu", "sans-serif", "Hiragino Sans"],
    secondary: [
      "'Inconsolata'",
      "'M PLUS 1p'",
      "system-ui",
      "Ubuntu",
      "sans-serif",
      "'Hiragino Sans'"
    ]
  },
  textSizes: {
    h1: "76px",
    h2: "54px",
    h3: "36px",
    h4: "28px",
    h5: "20px",
    h6: "18px",
    body: "16px",
    small: "14px",
    caption: "12px"
  },
  fontWeights: {
    regular: 300,
    bold: 600
  },
  leading: {
    h1: "122px",
    h2: "86px",
    h3: "58px",
    h4: "45px",
    h5: "32px",
    h6: "29px",
    body: "26px",
    small: "22px",
    caption: "19px"
  },
  tracking: {
    default: "1px"
  },
  textColors: colors,
  backgroundColors: colors,
  backgroundSize: {
    auto: "auto",
    cover: "cover",
    contain: "contain"
  },
  borderWidths: {
    "0": "0",
    "2": "2px",
    "4": "4px",
    thin: "0.5px",
    default: "1px"
  },
  borderColors: { ...colors, default: colors["border-grey"] },
  borderRadius: {
    none: "0",
    default: "4px",
    full: "9999px"
  },
  width: {
    auto: "auto",
    px: "1px",
    "1/2": "50%",
    "1/3": "33.33333%",
    "2/3": "66.66667%",
    "1/4": "25%",
    "3/4": "75%",
    "1/5": "20%",
    "2/5": "40%",
    "3/5": "60%",
    "4/5": "80%",
    "1/6": "16.66667%",
    "5/6": "83.33333%",
    full: "100%",
    screen: "100vw",
    box: "80px"
  },
  height: {
    auto: "auto",
    px: "1px",
    full: "100%",
    screen: "100vh",
    box: "80px"
  },
  maxWidth: {
    content: "400px"
  },
  minWidth: {
    "0": "0",
    full: "100%"
  },
  minHeight: {
    "0": "0",
    full: "100%",
    screen: "100vh"
  },
  padding: spacingsWithPx,
  margin: spacingsWithPx,
  negativeMargin: spacingsWithPx,
  shadows: {
    a: "0 2px 4px 0 rgba(2, 8, 2, 0.08)",
    b: "0 4px 8px 0 rgba(2, 8, 2, 0.08)",
    c: "0 8px 16px 0 rgba(2, 8, 2, 0.08)",
    d: "0 10px 24px 0 rgba(2, 8, 2, 0.08)",
    none: "none"
  },
  zIndex: {
    auto: "auto",
    "0": 0,
    "10": 10,
    "20": 20,
    "30": 30,
    "40": 40,
    "50": 50
  },
  opacity: {
    "0": "0",
    "10": ".10",
    "55": ".55",
    "80": ".80",
    "100": "1"
  },
  svgFill: {
    current: "currentColor"
  },
  svgStroke: {
    current: "currentColor"
  },
  modules: {
    appearance: [],
    backgroundAttachment: [],
    backgroundColors: ["hover", "focus"],
    backgroundPosition: [],
    backgroundRepeat: [],
    backgroundSize: [],
    borderCollapse: [],
    borderColors: ["hover", "focus"],
    borderRadius: [],
    borderStyle: [],
    borderWidths: ["focus"],
    cursor: [],
    display: [],
    flexbox: [],
    float: [],
    fonts: [],
    fontWeights: ["hover", "focus"],
    height: [],
    leading: [],
    lists: [],
    margin: [],
    maxHeight: [],
    maxWidth: [],
    minHeight: [],
    minWidth: [],
    negativeMargin: [],
    objectFit: [],
    opacity: ["hover"],
    outline: ["focus"],
    overflow: [],
    padding: [],
    pointerEvents: [],
    position: [],
    resize: [],
    shadows: ["hover", "focus"],
    svgFill: [],
    svgStroke: [],
    tableLayout: [],
    textAlign: [],
    textColors: ["hover", "focus"],
    textSizes: [],
    textStyle: ["hover", "focus"],
    tracking: [],
    userSelect: [],
    verticalAlign: [],
    visibility: [],
    whitespace: [],
    width: [],
    zIndex: []
  },
  plugins: [require("tailwindcss/plugins/container")({})],
  options: {
    prefix: "",
    important: false,
    separator: ":"
  }
};
