<Workflow class="WorkflowTrace" event="go" layout="startSrcAssocAnchorPos:1024,14;startX:285;endY:556;startY:22;endX:285" variables="exception">
   <TryCatch caption="Outer Try" layout="w:483;h:316;y:93;x:145" name="outerTry" variable="exception">
      <Try>
         <TryCatch caption="Inner Try" layout="w:225;h:172;y:36;srcAssocAnchorPos:1024,119;x:33" name="innerTry">
            <Try>
               <Action caption="Throw Exceptions" layout="y:45;x:52" name="throwBlock"><![CDATA[(import 'java.lang.IllegalStateException)
(import 'java.lang.NumberFormatException)
(import 'java.lang.UnsupportedOperationException)

(this'traceAppend "THROWBLOCK;")

(cond
   ((this'throwInnerEx) (throw (java.lang.IllegalStateException'new "Inner Ex")) )
   ((this'throwOuterEx) (throw (java.lang.NumberFormatException'new "Outer Ex")) )
   ((this'throwOuter2Ex) (throw (java.lang.UnsupportedOperationException'new "Outer2 Ex")))
   ((this'throwUncaughtEx) (error "Uncaught Ex") )
)

(this'traceAppend "END_THROWBLOCK;")
]]></Action>
               <Goto/>
            </Try>
            <Catch exception="java.lang.IllegalStateException" layout="assocAnchorPos:131072,105" name="catchInner">
               <Action caption="Catch Inner" layout="y:128;x:326" name="scrCatchInner"><![CDATA[(this'traceAppend "CAUGHT_INNER;")]]></Action>
               <Goto/>
            </Catch>
            <Finally><![CDATA[(this'traceAppend "F_INNER;")]]></Finally>
         </TryCatch>
         <Goto/>
         <Action caption="In Outer Try" layout="y:205;x:331" name="scrInOuterTry"><![CDATA[(this'traceAppend "RUNNING_IN_OUTER_TRY;")
(this'traceAppend "END_IN_OUTER_TRY;")]]></Action>
      </Try>
      <Catch exception="java.lang.NumberFormatException" name="catchOuter">
         <Action caption="Catch Outer" layout="y:242;x:737" name="scrCatchOuter"><![CDATA[(this'traceAppend "CAUGHT_OUTER;")]]></Action>
      </Catch>
      <Catch exception="java.lang.UnsupportedOperationException" name="catchOuter2">
         <Action caption="Catch Outer2" layout="y:312;x:747" name="scrCatchOuter2"><![CDATA[(this'traceAppend "CAUGHT_OUTER2;")]]></Action>
         <Goto next="scrInOuterTry"/>
      </Catch>
      <Finally><![CDATA[(this'traceAppend "F_OUTER;")]]></Finally>
   </TryCatch>
</Workflow>
