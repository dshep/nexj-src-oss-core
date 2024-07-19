<Workflow caption="Holiday Request" class="HRRequest" description="Workflow around a holiday request" layout="startX:285;endY:549;startY:7;endX:777">
   <Assignment caption="Review a holiday request" layout="y:64;x:220" name="Review by the manager" owner="(@ applicant manager)"/>
   <Decision layout="y:140;x:276" manual="true" name="branch on manager's decision">
      <Branch caption="Approve" name="if approved">
         <Action layout="y:229;x:247" name="set approved"><![CDATA[
            (this'approved #f)
            ]]></Action>
         <Join layout="y:307;x:19;srcAssoc0:334,562" name="escalationJoin" type="any">
            <Activity>
               <Join layout="y:0;x:243" name="approvedJoin">
                  <Activity>
                     <Assignment caption="Review an approved holiday request" name="Review by the HR" owner="(@ applicant manager)"/>
                  </Activity>
                  <Activity>
                     <Assignment caption="Your holiday request has been approved" layout="y:0;x:221" name="Review by the applicant" owner="(@ applicant)"/>
                  </Activity>
               </Join>
            </Activity>
            <Activity>
               <Timeout layout="y:21;x:0" name="approve by default if no action is taken ;-)" value="120000"/>
            </Activity>
         </Join>
      </Branch>
      <Branch caption="Disapprove" name="if disapproved">
         <Action layout="y:133;x:731" name="set disapproved"><![CDATA[
            (this'approved #f)
            ]]></Action>
         <Assignment caption="Your holiday request has been disapproved" layout="y:236;x:658" name="Applicant notification" owner="(@ applicant)"/>
      </Branch>
   </Decision>
</Workflow>
