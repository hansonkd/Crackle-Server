#Crackle Web Server

Purpose
----
The purpose of this webserver is to be based on snap but with a different "plugin" system. Snap's current plugin system is based snaplets. These snaplets are lensed based. This means when you write your application you have to create new lenses and then create a class instance if you wish to add automatic context to your application. 

For example... look at the generic application with basic snaplets added:

    data App = App
        { _heist       :: Snaplet (Heist App)
        , _foo         :: Snaplet Foo
        , _bar         :: Snaplet Bar
        , _companyName :: IORef B.ByteString
        }
    
    makeLenses ''App
    
    instance HasHeist App where
        heistLens = subSnaplet heist
        
    appInit :: SnapletInit App App
    appInit = makeSnaplet "myapp" "My example application" Nothing $ do
        hs <- nestSnaplet "heist" heist $ heistInit "templates"
        fs <- nestSnaplet "foo" foo $ fooInit
        bs <- nestSnaplet "" bar $ nameSnaplet "newname" $ barInit foo
        addRoutes [ ("/hello", writeText "hello world")
                  , ("/fooname", with foo namePage)
                  , ("/barname", with bar namePage)
                  , ("/company", companyHandler)
                  ]
        wrapSite (<|> heistServe)
        ref <- liftIO $ newIORef "fooCorp"
        return $ App hs fs bs ref
        
Here we referenced each snaplet twice in order to build our application. We also had to create an instance of HasHeist for App. If you nested this snaplet in another snaplet, you would have to define another instance of HasHeist for your parent snaplet. However, wouldn't it be nice to build an application like this:

    myApplication = (HeistShard :$: FooShard :$: BarShard)

That's it. No declaring new instances or manually nesting plugins. Now instead of using a lens to access the plugin, we just use a function that is predifined by the plugin.

    getHeist :: HasShard a HeistShard => a -> HeistShard
    ..
    getHeist myApplicaiton :: HeistShard

Not only that, but our new groups of plugins are also plugins themselves. Due to our data structure, if we nest plugins we will not have to define new instances.


Examining main.hs
----

We start out by creating new datatypes for our plugin objects

    data MainServer = MainServer | MainServerInit
        deriving (Show)

    data SubServer = SubServer | SubServerInit
        deriving (Show)

We want to make it convient for people to access SubServer (incase we ever want to put any information in it). So, we simply inherit from the class HasShard.

    class HasShard a SubServer => HasSubServer a where
        getSubServer :: a -> SubServer

    instance HasShard a SubServer => HasSubServer a where
        getSubServer = getShard

We can also make commands that we can run from the command line with our application's context. Note: This is in an IO monad and not a Snap Monad. So you do not have access to request objects and response objects

    printConfigCommand :: (Show a, HasBase a, HasSubServer a) => Command a
    printConfigCommand = Command {
            name    = "run",
            help    = Nothing,
            options = [],
            command = (\optDict -> do
                    app <- ask
                    liftIO $ getSubServer app
                )
        }
        

We create a shard with an option URL prefix the routes, commands, and its configuration function

    instance (Show b, HasBase b, HasSubServer b) => Shard MainServer b where
        urlPrefix _   = Just "/m"
        routes    _   = [packView IndexView]
        commands  _   = [printConfigCommand]
        runConfig _ _ = MainServer

In Crackle, views are class instances.

    data IndexView = IndexView
    instance HasSubServer b => ShardView IndexView b where
        url        _ = "/c"
        runRequest _ = do
                appState <- asks getSubServer
                liftIO $ print $ appState
                writeBS "HelloWorld!"

Create our second Shard

    instance Shard SubServer b where
        urlPrefix _   = Nothing
        routes    _   = []
        commands  _   = []
        runConfig _ _ = SubServer

Glue together the rest of our application:

    myApplication :: (BaseApplication :$: (MainServer :$: SubServer) )
    myApplication =  (BaseApplicationInit :$: (MainServerInit :$: SubServerInit))
    
    myConfig :: CrackleConfig
    myConfig = M.fromList [("SOMECONFIG", "OTHERCONFIG")]
    
    main :: IO ()
    main = do
        let (app, commands) = initApplication myConfig myApplication 
        (_, s) <- runCrackle (runCommands $ commands) app
        return ()



