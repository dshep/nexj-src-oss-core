<Workflow caption="PersistedLoop" class="Contact" description="Workflow that loops, persisting each time in the loop." event="start" layout="startX:332;endY:639;startY:44;endX:639" variables="item addrs">
   <Action caption="Begin" layout="y:102;x:312" name="scrBegin"><![CDATA[(logger'debug "Starting loop on " this)
(set! addrs (this'addresses))]]></Action>
   <Loop collection="addrs" layout="w:298;h:201;y:136;x:461" name="address Loop" variable="item">
      <Queue layout="y:19;x:31" name="queue">
         <ClassEvent event="process" name="Process">
            <Action caption="Process" layout="y:29;x:180" name="scrProcess"><![CDATA[(logger'debug "Processing item " item)
(item'city "Hong Kong")]]></Action>
         </ClassEvent>
         <ClassEvent event="break" name="Break">
            <Action caption="Break from Loop" layout="y:103;x:60;srcAssoc0:540,395" name="scrBreak"><![CDATA[(logger'debug "Breaking from loop on " this " at item " item)]]></Action>
            <Goto next="scrEnd"/>
         </ClassEvent>
      </Queue>
   </Loop>
   <Action caption="End" layout="y:432;x:636" name="scrEnd"><![CDATA[(logger'debug "Done loop on " this)]]></Action>
</Workflow>

