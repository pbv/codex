<bind tag="icon-folder"><img src="/static/icons/16x16/folder.png"></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/text-editor.png"/></bind>

<apply template="base">
<h1>/<file-path/></h1>

<table class="filelist">
  <tr>
    <td/> <th>File name</th> <th>Type</th>  <th>Last Modified</th>
  </tr> 
  <file-list>
    <tr>
      <td>
	<if-text><icon-editor/></if-text>
	<if-dir><icon-folder/></if-dir>
      </td>
      <td class="filename">
	<if-text>
	  <a href="/edit/${file-path}"><file-name/></a>
	  <else/>
	  <a href="/browse/${file-path}"><file-name/></a>
	</if-text>
      </td> 
      <td class="filetype"><file-type/></td> 
      <td class="filemodified"><file-modified/></td>
    </tr>
  </file-list>
</table>

</apply>
