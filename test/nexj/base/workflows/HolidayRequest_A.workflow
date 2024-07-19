<Workflow caption="Holiday Request" class="HRRequest" description="Workflow around a holiday request" event="create" layout="startX:281;endY:425;startY:4;endX:628" variables="cnt">
   <Assignment condition="(not (null? (@ approved)))" event="update" layout="y:59;x:211" name="Review by the manager" owner="(@ applicant manager)" title="&quot;Review a holiday request&quot;"/>
   <Decision layout="y:114;x:271" name="branch on manager's decision">
      <Branch condition="(@ approved)" name="if approved">
         <Join layout="y:201;srcAssocAnchorPos:1024,509;x:8;srcAssoc0:324,433" name="escalationJoin" type="any">
            <Activity>
               <Join layout="y:0;x:244" name="approvedJoin">
                  <Activity>
                     <Assignment layout="y:9;x:9" name="Review by the HR" owner="(@ applicant manager)" title="&quot;Review an approved holiday request&quot;"/>
                  </Activity>
                  <Activity>
                     <Assignment layout="y:9;x:144" name="Review by the applicant" owner="(@ applicant)" title="&quot;Your holiday request has been approved&quot;"/>
                  </Activity>
               </Join>
            </Activity>
            <Activity>
               <Timeout layout="srcAssocAnchorPos:1024,16;y:25;srcAssoc0:131,303;x:4" name="approve by default if no action is taken ;-)" value="120000"/>
            </Activity>
         </Join>
         <Action layout="y:425;x:429" name="Init counter"><![CDATA[(set! cnt 0)]]></Action>
         <Increment name="Increment counter" variable="cnt"/>
         <Increment delta="2" name="Increment counter by 2" variable="cnt"/>
         <Action layout="y:492;x:576" name="Log counter"><![CDATA[(logger'debug "cnt =" cnt)]]></Action>
      </Branch>
      <Branch layout="assocAnchorPos:131072,11" name="if disapproved">
         <Assignment layout="srcAssocAnchorPos:1024,110;y:115;x:575" name="Applicant notification" owner="(@ applicant)" title="&quot;Your holiday request has been disapproved&quot;"/>
      </Branch>
   </Decision>
</Workflow>
