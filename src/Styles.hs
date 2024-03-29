module Styles (
	 styleDifficulty,
	 styleDifficultySelected,
	 styleSelected, 
	 styleUnSelected, 
	 styleAutoPlay,
	 styleSelectAutoPlay,
) where

styleDifficulty, styleDifficultySelected, styleSelected, styleUnSelected, styleAutoPlay, styleSelectAutoPlay:: [(String, String)]
styleDifficulty = 
         [("background-color", "#4b752c"),
          ("border", "groove"), 
          ("border-color", "#416f1f"),
          ("border-width", "thick"),
          ("border-radius", "10px"),
          ("color", "white"),  
          ("width", "100px"), 
          ("height", "45px"),
          ("text-align", "center"),
          ("text-decoration", "none"),
          ("display", "inline-block"),
          ("font-size", "auto"),
          ("margin", "5px 5px"),
          ("cursor", "pointer"),
          ("font-weight", "normal")]
styleDifficultySelected = 
         [("background-color", "#315318"),
          ("border", "groove"), 
          ("border-color", "#416f1f"),
          ("border-width", "thick"),
          ("border-radius", "10px"),
          ("color", "white"),  
          ("width", "100px"), 
          ("height", "45px"),
          ("text-align", "center"),
          ("text-decoration", "none"),
          ("display", "inline-block"),
          ("font-size", "auto"),
          ("margin", "5px 5px"),
          ("cursor", "pointer"),
          ("font-weight", "normal")]
styleAutoPlay = 
         [("background-color", "white"),
          ("border", "groove"), 
          ("border-color", "#303030"),
          ("border-width", "thick"),
          ("border-radius", "10px"),
          ("color", "#303030"),  
          ("width", "200px"), 
          ("height", "45px"),
          ("text-align", "center"),
          ("text-decoration", "none"),
          ("display", "inline-block"),
          ("font-size", "auto"),
          ("margin", "5px 5px"),
          ("cursor", "pointer"),
          ("font-weight", "bold")]
styleSelectAutoPlay = 
         [("background-color", "#f0f0f0"),
          ("border", "groove"), 
          ("border-color", "#303030"),
          ("border-width", "thick"),
          ("border-radius", "10px"),
          ("color", "#303030"),  
          ("width", "200px"), 
          ("height", "45px"),
          ("text-align", "center"),
          ("text-decoration", "none"),
          ("display", "inline-block"),
          ("font-size", "auto"),
          ("margin", "5px 5px"),
          ("cursor", "pointer"),
          ("font-weight", "bold")]
styleSelected = 
         [("background-color", "#4b752c"),
          ("border", "ridge"), 
          ("border-color", "white"),
          ("border-width", "thick"),
          ("border-radius", "10px"),
          ("color", "white"),  
          ("width", "100px"), 
          ("height", "45px"),
          ("text-align", "center"),
          ("text-decoration", "none"),
          ("display", "inline-block"),
          ("font-size", "auto"),
          ("margin", "5px 5px"),
          ("cursor", "default"),
          ("font-weight", "bold")]
styleUnSelected = 
         [("background-color", "#63ab30"), 
          ("border", "groove"), 
          ("border-color", "#416f1f"),
          ("border-width", "thick"),
          ("border-radius", "10px"),
          ("color", "black"),  
          ("width", "100px"), 
          ("height", "45px"),
          ("text-align", "center"),
          ("text-decoration", "none"),
          ("display", "inline-block"),
          ("font-size", "16px"),
          ("margin", "5px 5px"),
          ("cursor", "pointer"),
          ("font-weight", "normal")]
