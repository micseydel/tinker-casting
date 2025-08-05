```
sbt compile
error:
  bad constant pool index: 0 at pos: 49176
     while compiling: <no file>
        during phase: globalPhase=<no phase>, enteringPhase=<some phase>
     library version: version 2.12.17
    compiler version: version 2.12.17
  reconstructed args: -classpath /Users/micseydel/.sbt/boot/scala-2.12.17/lib/scala-library.jar -Yrangepos

  last tree to typer: EmptyTree
       tree position: <unknown>
            tree tpe: <notype>
              symbol: null
           call site: <none> in <none>

== Source file context for tree position ==

error:
  bad constant pool index: 0 at pos: 49176
     while compiling: <no file>
        during phase: globalPhase=<no phase>, enteringPhase=<some phase>
     library version: version 2.12.17
    compiler version: version 2.12.17
  reconstructed args: -classpath /Users/micseydel/.sbt/boot/scala-2.12.17/lib/scala-library.jar -Yrangepos

  last tree to typer: EmptyTree
       tree position: <unknown>
            tree tpe: <notype>
              symbol: null
           call site: <none> in <none>

== Source file context for tree position ==

Exception in thread "sbt-parser-init-thread" java.lang.ExceptionInInitializerError
	at sbt.internal.parser.SbtParserInit$$anon$2.run(SbtParser.scala:191)
Caused by: scala.reflect.internal.FatalError: 
  bad constant pool index: 0 at pos: 49176
     while compiling: <no file>
        during phase: globalPhase=<no phase>, enteringPhase=<some phase>
     library version: version 2.12.17
    compiler version: version 2.12.17
  reconstructed args: -classpath /Users/micseydel/.sbt/boot/scala-2.12.17/lib/scala-library.jar -Yrangepos

  last tree to typer: EmptyTree
       tree position: <unknown>
            tree tpe: <notype>
              symbol: null
           call site: <none> in <none>

== Source file context for tree position ==


	at scala.reflect.internal.Reporting.abort(Reporting.scala:69)
	at scala.reflect.internal.Reporting.abort$(Reporting.scala:65)
	at scala.reflect.internal.SymbolTable.abort(SymbolTable.scala:28)
	at scala.tools.nsc.symtab.classfile.ClassfileParser$ConstantPool.errorBadIndex(ClassfileParser.scala:385)
	at scala.tools.nsc.symtab.classfile.ClassfileParser$ConstantPool.getExternalName(ClassfileParser.scala:249)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.readParamNames$1(ClassfileParser.scala:828)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.parseAttribute$1(ClassfileParser.scala:834)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.$anonfun$parseAttributes$7(ClassfileParser.scala:908)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.parseAttributes(ClassfileParser.scala:908)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.parseMethod(ClassfileParser.scala:611)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.$anonfun$parseClass$4(ClassfileParser.scala:534)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.parseClass(ClassfileParser.scala:534)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.$anonfun$parse$2(ClassfileParser.scala:160)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.$anonfun$parse$1(ClassfileParser.scala:146)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.parse(ClassfileParser.scala:129)
	at scala.tools.nsc.symtab.SymbolLoaders$ClassfileLoader.doComplete(SymbolLoaders.scala:343)
	at scala.tools.nsc.symtab.SymbolLoaders$SymbolLoader.complete(SymbolLoaders.scala:250)
	at scala.reflect.internal.Symbols$Symbol.completeInfo(Symbols.scala:1542)
	at scala.reflect.internal.Symbols$Symbol.info(Symbols.scala:1514)
	at scala.reflect.internal.Definitions.scala$reflect$internal$Definitions$$enterNewMethod(Definitions.scala:49)
	at scala.reflect.internal.Definitions$DefinitionsClass.String_$plus$lzycompute(Definitions.scala:1134)
	at scala.reflect.internal.Definitions$DefinitionsClass.String_$plus(Definitions.scala:1134)
	at scala.reflect.internal.Definitions$DefinitionsClass.syntheticCoreMethods$lzycompute(Definitions.scala:1438)
	at scala.reflect.internal.Definitions$DefinitionsClass.syntheticCoreMethods(Definitions.scala:1420)
	at scala.reflect.internal.Definitions$DefinitionsClass.symbolsNotPresentInBytecode$lzycompute(Definitions.scala:1450)
	at scala.reflect.internal.Definitions$DefinitionsClass.symbolsNotPresentInBytecode(Definitions.scala:1450)
	at scala.reflect.internal.Definitions$DefinitionsClass.init(Definitions.scala:1506)
	at scala.tools.nsc.Global$Run.<init>(Global.scala:1214)
	at sbt.internal.parser.SbtParser$.<init>(SbtParser.scala:141)
	at sbt.internal.parser.SbtParser$.<clinit>(SbtParser.scala)
	... 1 more
[info] welcome to sbt 1.8.2 (Homebrew Java 22.0.2)
java.lang.NoClassDefFoundError: Could not initialize class sbt.internal.parser.SbtParser$
	at sbt.internal.parser.SbtParser.splitExpressions(SbtParser.scala:247)
	at sbt.internal.parser.SbtParser.<init>(SbtParser.scala:236)
	at sbt.internal.EvaluateConfigurations$.splitExpressions(EvaluateConfigurations.scala:289)
	at sbt.internal.EvaluateConfigurations$.parseConfiguration(EvaluateConfigurations.scala:98)
	at sbt.internal.EvaluateConfigurations$.evaluateSbtFile(EvaluateConfigurations.scala:147)
	at sbt.internal.Load$.loadSettingsFile$1(Load.scala:1118)
	at sbt.internal.Load$.$anonfun$discoverProjects$2(Load.scala:1128)
	at scala.collection.MapLike.getOrElse(MapLike.scala:131)
	at scala.collection.MapLike.getOrElse$(MapLike.scala:129)
	at scala.collection.AbstractMap.getOrElse(Map.scala:65)
	at sbt.internal.Load$.memoLoadSettingsFile$1(Load.scala:1127)
	at sbt.internal.Load$.$anonfun$discoverProjects$4(Load.scala:1135)
	at scala.collection.immutable.List.map(List.scala:293)
	at sbt.internal.Load$.loadFiles$1(Load.scala:1135)
	at sbt.internal.Load$.discoverProjects(Load.scala:1149)
	at sbt.internal.Load$.discover$1(Load.scala:901)
	at sbt.internal.Load$.loadTransitive(Load.scala:955)
	at sbt.internal.Load$.loadProjects$1(Load.scala:738)
	at sbt.internal.Load$.$anonfun$loadUnit$12(Load.scala:741)
	at sbt.internal.Load$.timed(Load.scala:1406)
	at sbt.internal.Load$.$anonfun$loadUnit$1(Load.scala:741)
	at sbt.internal.Load$.timed(Load.scala:1406)
	at sbt.internal.Load$.loadUnit(Load.scala:694)
	at sbt.internal.Load$.$anonfun$builtinLoader$4(Load.scala:492)
	at sbt.internal.BuildLoader$.$anonfun$componentLoader$5(BuildLoader.scala:180)
	at sbt.internal.BuildLoader.apply(BuildLoader.scala:245)
	at sbt.internal.Load$.loadURI$1(Load.scala:554)
	at sbt.internal.Load$.loadAll(Load.scala:570)
	at sbt.internal.Load$.loadURI(Load.scala:500)
	at sbt.internal.Load$.load(Load.scala:479)
	at sbt.internal.Load$.$anonfun$apply$1(Load.scala:241)
	at sbt.internal.Load$.timed(Load.scala:1406)
	at sbt.internal.Load$.apply(Load.scala:241)
	at sbt.internal.Load$.buildPluginDefinition(Load.scala:1323)
	at sbt.internal.Load$.buildPlugins(Load.scala:1253)
	at sbt.internal.Load$.plugins(Load.scala:1232)
	at sbt.internal.Load$.$anonfun$loadUnit$2(Load.scala:700)
	at sbt.internal.Load$.timed(Load.scala:1406)
	at sbt.internal.Load$.$anonfun$loadUnit$1(Load.scala:700)
	at sbt.internal.Load$.timed(Load.scala:1406)
	at sbt.internal.Load$.loadUnit(Load.scala:694)
	at sbt.internal.Load$.$anonfun$builtinLoader$4(Load.scala:492)
	at sbt.internal.BuildLoader$.$anonfun$componentLoader$5(BuildLoader.scala:180)
	at sbt.internal.BuildLoader.apply(BuildLoader.scala:245)
	at sbt.internal.Load$.loadURI$1(Load.scala:554)
	at sbt.internal.Load$.loadAll(Load.scala:570)
	at sbt.internal.Load$.loadURI(Load.scala:500)
	at sbt.internal.Load$.load(Load.scala:479)
	at sbt.internal.Load$.$anonfun$apply$1(Load.scala:241)
	at sbt.internal.Load$.timed(Load.scala:1406)
	at sbt.internal.Load$.apply(Load.scala:241)
	at sbt.internal.Load$.defaultLoad(Load.scala:56)
	at sbt.BuiltinCommands$.liftedTree1$1(Main.scala:961)
	at sbt.BuiltinCommands$.doLoadProject(Main.scala:961)
	at sbt.BuiltinCommands$.$anonfun$loadProjectImpl$2(Main.scala:914)
	at sbt.Command$.$anonfun$applyEffect$4(Command.scala:150)
	at sbt.Command$.$anonfun$applyEffect$2(Command.scala:145)
	at sbt.Command$.process(Command.scala:189)
	at sbt.MainLoop$.$anonfun$processCommand$5(MainLoop.scala:245)
	at scala.Option.getOrElse(Option.scala:189)
	at sbt.MainLoop$.process$1(MainLoop.scala:245)
	at sbt.MainLoop$.processCommand(MainLoop.scala:278)
	at sbt.MainLoop$.$anonfun$next$5(MainLoop.scala:163)
	at sbt.State$StateOpsImpl$.runCmd$1(State.scala:289)
	at sbt.State$StateOpsImpl$.process$extension(State.scala:325)
	at sbt.MainLoop$.$anonfun$next$4(MainLoop.scala:163)
	at sbt.internal.util.ErrorHandling$.wideConvert(ErrorHandling.scala:23)
	at sbt.MainLoop$.next(MainLoop.scala:163)
	at sbt.MainLoop$.run(MainLoop.scala:144)
	at sbt.MainLoop$.$anonfun$runWithNewLog$1(MainLoop.scala:119)
	at sbt.io.Using.apply(Using.scala:27)
	at sbt.MainLoop$.runWithNewLog(MainLoop.scala:112)
	at sbt.MainLoop$.runAndClearLast(MainLoop.scala:66)
	at sbt.MainLoop$.runLoggedLoop(MainLoop.scala:51)
	at sbt.MainLoop$.runLogged(MainLoop.scala:42)
	at sbt.StandardMain$.runManaged(Main.scala:223)
	at sbt.xMain$.$anonfun$run$11(Main.scala:133)
	at scala.util.DynamicVariable.withValue(DynamicVariable.scala:62)
	at scala.Console$.withIn(Console.scala:230)
	at sbt.internal.util.Terminal$.withIn(Terminal.scala:578)
	at sbt.internal.util.Terminal$.$anonfun$withStreams$1(Terminal.scala:358)
	at scala.util.DynamicVariable.withValue(DynamicVariable.scala:62)
	at scala.Console$.withOut(Console.scala:167)
	at sbt.internal.util.Terminal$.$anonfun$withOut$2(Terminal.scala:568)
	at scala.util.DynamicVariable.withValue(DynamicVariable.scala:62)
	at scala.Console$.withErr(Console.scala:196)
	at sbt.internal.util.Terminal$.withOut(Terminal.scala:568)
	at sbt.internal.util.Terminal$.withStreams(Terminal.scala:358)
	at sbt.xMain$.withStreams$1(Main.scala:87)
	at sbt.xMain$.run(Main.scala:121)
	at java.base/jdk.internal.reflect.DirectMethodHandleAccessor.invoke(DirectMethodHandleAccessor.java:103)
	at java.base/java.lang.reflect.Method.invoke(Method.java:580)
	at sbt.internal.XMainConfiguration.run(XMainConfiguration.java:57)
	at sbt.xMain.run(Main.scala:46)
	at xsbt.boot.Launch$.$anonfun$run$1(Launch.scala:149)
	at xsbt.boot.Launch$.withContextLoader(Launch.scala:176)
	at xsbt.boot.Launch$.run(Launch.scala:149)
	at xsbt.boot.Launch$.$anonfun$apply$1(Launch.scala:44)
	at xsbt.boot.Launch$.launch(Launch.scala:159)
	at xsbt.boot.Launch$.apply(Launch.scala:44)
	at xsbt.boot.Launch$.apply(Launch.scala:21)
	at xsbt.boot.Boot$.runImpl(Boot.scala:78)
	at xsbt.boot.Boot$.run(Boot.scala:73)
	at xsbt.boot.Boot$.main(Boot.scala:21)
	at xsbt.boot.Boot.main(Boot.scala)
Caused by: java.lang.ExceptionInInitializerError: Exception scala.reflect.internal.FatalError: 
  bad constant pool index: 0 at pos: 49176
     while compiling: <no file>
        during phase: globalPhase=<no phase>, enteringPhase=<some phase>
     library version: version 2.12.17
    compiler version: version 2.12.17
  reconstructed args: -classpath /Users/micseydel/.sbt/boot/scala-2.12.17/lib/scala-library.jar -Yrangepos

  last tree to typer: EmptyTree
       tree position: <unknown>
            tree tpe: <notype>
              symbol: null
           call site: <none> in <none>

== Source file context for tree position ==

 [in thread "sbt-parser-init-thread"]
	at scala.reflect.internal.Reporting.abort(Reporting.scala:69)
	at scala.reflect.internal.Reporting.abort$(Reporting.scala:65)
	at scala.reflect.internal.SymbolTable.abort(SymbolTable.scala:28)
	at scala.tools.nsc.symtab.classfile.ClassfileParser$ConstantPool.errorBadIndex(ClassfileParser.scala:385)
	at scala.tools.nsc.symtab.classfile.ClassfileParser$ConstantPool.getExternalName(ClassfileParser.scala:249)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.readParamNames$1(ClassfileParser.scala:828)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.parseAttribute$1(ClassfileParser.scala:834)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.$anonfun$parseAttributes$7(ClassfileParser.scala:908)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.parseAttributes(ClassfileParser.scala:908)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.parseMethod(ClassfileParser.scala:611)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.$anonfun$parseClass$4(ClassfileParser.scala:534)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.parseClass(ClassfileParser.scala:534)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.$anonfun$parse$2(ClassfileParser.scala:160)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.$anonfun$parse$1(ClassfileParser.scala:146)
	at scala.tools.nsc.symtab.classfile.ClassfileParser.parse(ClassfileParser.scala:129)
	at scala.tools.nsc.symtab.SymbolLoaders$ClassfileLoader.doComplete(SymbolLoaders.scala:343)
	at scala.tools.nsc.symtab.SymbolLoaders$SymbolLoader.complete(SymbolLoaders.scala:250)
	at scala.reflect.internal.Symbols$Symbol.completeInfo(Symbols.scala:1542)
	at scala.reflect.internal.Symbols$Symbol.info(Symbols.scala:1514)
	at scala.reflect.internal.Definitions.scala$reflect$internal$Definitions$$enterNewMethod(Definitions.scala:49)
	at scala.reflect.internal.Definitions$DefinitionsClass.String_$plus$lzycompute(Definitions.scala:1134)
	at scala.reflect.internal.Definitions$DefinitionsClass.String_$plus(Definitions.scala:1134)
	at scala.reflect.internal.Definitions$DefinitionsClass.syntheticCoreMethods$lzycompute(Definitions.scala:1438)
	at scala.reflect.internal.Definitions$DefinitionsClass.syntheticCoreMethods(Definitions.scala:1420)
	at scala.reflect.internal.Definitions$DefinitionsClass.symbolsNotPresentInBytecode$lzycompute(Definitions.scala:1450)
	at scala.reflect.internal.Definitions$DefinitionsClass.symbolsNotPresentInBytecode(Definitions.scala:1450)
	at scala.reflect.internal.Definitions$DefinitionsClass.init(Definitions.scala:1506)
	at scala.tools.nsc.Global$Run.<init>(Global.scala:1214)
	at sbt.internal.parser.SbtParser$.<init>(SbtParser.scala:141)
	at sbt.internal.parser.SbtParser$.<clinit>(SbtParser.scala)
	at sbt.internal.parser.SbtParserInit$$anon$2.run(SbtParser.scala:191)
[error] java.lang.NoClassDefFoundError: Could not initialize class sbt.internal.parser.SbtParser$
[error] Use 'last' for the full log.
```
