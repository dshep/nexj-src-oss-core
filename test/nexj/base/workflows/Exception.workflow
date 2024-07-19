<Workflow class="HRRequest" variables="exception" layout="startSrcAssocAnchorPos:1024,14;startX:285;endY:772;startY:22;endX:286">
   <TryCatch name="tryCatch" variable="exception" layout="w:542;h:607;y:93;x:145">
      <Try>
         <Action name="runtime" layout="y:34;x:113"><![CDATA[
(this'comment "runtime_thrown")
(import 'java.lang.RuntimeException)
]]></Action>
         <TryCatch name="fallThrough" layout="w:225;h:172;y:96;srcAssocAnchorPos:1024,119;x:44">
            <Try>
               <Action name="throwRuntime" layout="y:45;x:52"><![CDATA[
                 (throw (java.lang.RuntimeException'new "runtime"))
            ]]></Action>
            </Try>
            <Catch exception="nexj.core.integration.IntegrationException" layout="assoc0:457,280;assoc1:457,785"/>
         </TryCatch>
         <Goto/>
         <Action name="scripting" layout="y:165;srcAssocAnchorPos:16384,357;x:353"><![CDATA[
(when (equal? (@ comment) "runtime_caught")
   (this'comment "scripting_thrown")
   (error "scripting")
)
]]></Action>
         <Goto/>
         <Action name="done" layout="y:352;x:370"><![CDATA[
(this'comment "done")
(logger'info "Done")
]]></Action>
      </Try>
      <Catch exception="nexj.core.integration.IntegrationException" layout="assoc0:805,419;assoc1:808,790"/>
      <Catch exception="nexj.core.scripting.ScriptingException">
         <Action name="caught_scripting" layout="y:270;x:802"><![CDATA[
         (this'comment "scripting_caught")
         ]]></Action>
         <Goto next="done"/>
      </Catch>
      <Catch condition="(and (equal? (@ comment) &quot;runtime_thrown&quot;) (equal? (exception'message) &quot;runtime&quot;))" layout="assocAnchorPos:16384,461">
         <Action name="caught_runtime" layout="y:127;x:793"><![CDATA[
         (this'comment "runtime_caught")
         ]]></Action>
         <Goto next="scripting"/>
      </Catch>
   </TryCatch>
</Workflow>
