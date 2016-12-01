<bind tag="icon-text-html"><img src="/static/icons/16x16/text-html.png"/></bind>
<bind tag="icon-folder"><img src="/static/icons/16x16/folder.png"></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/text-editor.png"/></bind>

<apply template="base">


<h1>/<request-path/></h1>

<table class="filelist">
  <tr>
    <td/> <th>File name</th> <th>Type</th>  <th>Last Modified</th>
  </tr> 
  <file-list>
    <tr>
      <td><if-edit><icon-editor/></if-edit></td>
      <td class="filename">
	<if-edit>
	  <a href="/edit/${request-path}"><file-name/></a>
	  <else/>
	  <a href="/browse/${request-path}"><file-name/></a>
	</if-edit>
      </td> 
      <td class="filetype"><file-type/></td> 
      <td class="filemodified"><file-modified/></td>
    </tr>
  </file-list>
</table>

</apply>
