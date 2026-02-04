<bind tag="icon-folder"><img src="/static/icons/16x16/folder.png"></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/text-editor.png"/></bind>
<bind tag="icon-warning"><img src="/static/icons/16x16/warning.png"/></bind>
<apply template="_base">
  <apply template="_browse"/>

  <h1>/<file-path/></h1>

  <table class="filelist">
    <tr>
      <td/> <th>Name</th> <th>Type</th>  <th>Modified</th>
    </tr>
    <file-list>
      <tr>
	<td>
	  <if-text><icon-editor/></if-text>
	  <if-dir><icon-folder/></if-dir>
	</td>
	<td class="filename">
	  <a href="${file-url}"><file-name/></a>
	</td>
	<td class="filetype"><file-type/></td>
	<td class="filemodified"><file-modified/></td>
      </tr>
    </file-list>
  </table>
  <hr/>
  
  
  <form action="${file-url}" enctype="multipart/form-data" method="POST">
    <input type="hidden" name="_method" value="POST"/>
    <input type="file" name="datafile" required="required"/>
    <input type="submit" value="Upload file"/>
  </form>
  <div class="warnings">
    <dl>
      <message-list>
	<dd><icon-warning/>&nbsp;<message/></dd>
      </message-list>
    </dl>
  </div>
</apply>
