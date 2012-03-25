{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
import Yesod.Core (mkYesodData)
import Yesod.Routes.Parse (parseRoutesFile)
import Language.Haskell.TH (pprint, runQ)

th = mkYesodData "Cwl" $(parseRoutesFile "../config/routes")
main = runQ th >>= putStrLn.pprint

{-
 - The result looks like below
 -
instance Yesod.Routes.Class.RenderRoute Cwl
    where data Yesod.Routes.Class.Route Cwl
              = StaticR (Yesod.Routes.Class.Route Static)
              | AuthR (Yesod.Routes.Class.Route Auth)
              | FaviconR
              | RobotsR
              | RootR
              | EchoR Text
              | MirrorR
              | BlogR
              | ArticleR ArticleId
              deriving (GHC.Show.Show, GHC.Classes.Eq, GHC.Read.Read)
          renderRoute (StaticR sub_0) = (\(a_1,
                                           b_2) -> (Data.Text.pack "static" GHC.Types.: a_1,
                                                    b_2)) (Yesod.Routes.Class.renderRoute sub_0)
          renderRoute (AuthR sub_3) = (\(a_4,
                                         b_5) -> (Data.Text.pack "auth" GHC.Types.: a_4,
                                                  b_5)) (Yesod.Routes.Class.renderRoute sub_3)
          renderRoute (FaviconR) = (Data.Text.pack "favicon.ico" GHC.Types.: [],
                                    [])
          renderRoute (RobotsR) = (Data.Text.pack "robots.txt" GHC.Types.: [],
                                   [])
          renderRoute (RootR) = ([], [])
          renderRoute (EchoR dyn_6) = (Data.Text.pack "echo" GHC.Types.: (Web.PathPieces.toPathPiece dyn_6 GHC.Types.: []),
                                       [])
          renderRoute (MirrorR) = (Data.Text.pack "mirror" GHC.Types.: [],
                                   [])
          renderRoute (BlogR) = (Data.Text.pack "blog" GHC.Types.: [], [])
          renderRoute (ArticleR dyn_7) = (Data.Text.pack "blog" GHC.Types.: (Web.PathPieces.toPathPiece dyn_7 GHC.Types.: []),
                                          [])
type Handler = Yesod.Handler.GHandler Cwl Cwl
type Widget = Yesod.Widget.GWidget Cwl Cwl ()
resourcesCwl :: [Yesod.Routes.TH.Types.Resource GHC.Base.String]
resourcesCwl = [Yesod.Routes.TH.Types.Resource "StaticR" [(GHC.Types.True,
                                                           Yesod.Routes.TH.Types.Static "static")] (Yesod.Routes.TH.Types.Subsite ['S',
                                                                                                                                   't',
                                                                                                                                   'a',
                                                                                                                                   't',
                                                                                                                                   'i',
                                                                                                                                   'c'] "getStatic"),
                Yesod.Routes.TH.Types.Resource "AuthR" [(GHC.Types.True,
                                                         Yesod.Routes.TH.Types.Static "auth")] (Yesod.Routes.TH.Types.Subsite ['A',
                                                                                                                               'u',
                                                                                                                               't',
                                                                                                                               'h'] "getAuth"),
                Yesod.Routes.TH.Types.Resource "FaviconR" [(GHC.Types.True,
                                                            Yesod.Routes.TH.Types.Static "favicon.ico")] (Yesod.Routes.TH.Types.Methods Data.Maybe.Nothing [['G',
                                                                                                                                                             'E',
                                                                                                                                                             'T']]),
                Yesod.Routes.TH.Types.Resource "RobotsR" [(GHC.Types.True,
                                                           Yesod.Routes.TH.Types.Static "robots.txt")] (Yesod.Routes.TH.Types.Methods Data.Maybe.Nothing [['G',
                                                                                                                                                           'E',
                                                                                                                                                           'T']]),
                Yesod.Routes.TH.Types.Resource "RootR" [] (Yesod.Routes.TH.Types.Methods Data.Maybe.Nothing [['G',
                                                                                                              'E',
                                                                                                              'T']]),
                Yesod.Routes.TH.Types.Resource "EchoR" [(GHC.Types.True,
                                                         Yesod.Routes.TH.Types.Static "echo"),
                                                        (GHC.Types.True,
                                                         Yesod.Routes.TH.Types.Dynamic ['T',
                                                                                        'e',
                                                                                        'x',
                                                                                        't'])] (Yesod.Routes.TH.Types.Methods Data.Maybe.Nothing [['G',
                                                                                                                                                   'E',
                                                                                                                                                   'T']]),
                Yesod.Routes.TH.Types.Resource "MirrorR" [(GHC.Types.True,
                                                           Yesod.Routes.TH.Types.Static "mirror")] (Yesod.Routes.TH.Types.Methods Data.Maybe.Nothing [['G',
                                                                                                                                                       'E',
                                                                                                                                                       'T'],
                                                                                                                                                      ['P',
                                                                                                                                                       'O',
                                                                                                                                                       'S',
                                                                                                                                                       'T']]),
                Yesod.Routes.TH.Types.Resource "BlogR" [(GHC.Types.True,
                                                         Yesod.Routes.TH.Types.Static "blog")] (Yesod.Routes.TH.Types.Methods Data.Maybe.Nothing [['G',
                                                                                                                                                   'E',
                                                                                                                                                   'T'],
                                                                                                                                                  ['P',
                                                                                                                                                   'O',
                                                                                                                                                   'S',
                                                                                                                                                   'T']]),
                Yesod.Routes.TH.Types.Resource "ArticleR" [(GHC.Types.True,
                                                            Yesod.Routes.TH.Types.Static "blog"),
                                                           (GHC.Types.True,
                                                            Yesod.Routes.TH.Types.Dynamic ['A',
                                                                                           'r',
                                                                                           't',
                                                                                           'i',
                                                                                           'c',
                                                                                           'l',
                                                                                           'e',
                                                                                           'I',
                                                                                           'd'])] (Yesod.Routes.TH.Types.Methods Data.Maybe.Nothing [['G',
                                                                                                                                                      'E',
                                                                                                                                                      'T']])]
-}
