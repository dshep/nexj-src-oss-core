<Workflow class="Patient" event="triggerExceptionStart" layout="startSrcAssocAnchorPos:1024,14;startX:293;endY:282;startY:6;endX:297">
   <TryCatch layout="w:196;h:132;y:84;x:204" name="tryCatch">
      <Try>
         <Action caption="Throw Error" event="triggerException" layout="y:39;x:49" name="scrThrow"><![CDATA[(error "Error")]]></Action>
      </Try>
      <Catch layout="w:150;h:100;y:84;x:450" name="catch">
         <Action caption="Catch Error" layout="y:10;x:10" name="scrCatch"/>
      </Catch>
   </TryCatch>
</Workflow>
