# 关于这项服务的使用说明
这是一个获取SAP的ALV报表和底表数据的HTTP接口服务。  
  
它以 ICF 服务的形式提供此接口，在此系统中，此服务已分配给 ICF 服务[/sap/bc/zsicf_bi](http://vhcala4hci:50000/sap/bc/zsicf_bi "调用地址")。  
## 部署
通过abapgit部署

## 权限检查
该服务包含一个对名为 ZBI_AUTH 的自定义授权对象的AUTHORITY_CHECK调用，用于验证用户是否可以访问事务码或者底表,需要为每个访问该服务的用户都创建一个单独的角色，其中只有允许他访问的功能。

**权限对象**：

![pF2QXjg.jpg](https://s21.ax1x.com/2024/03/16/pF2QXjg.jpg)

**权限角色**：

![pF2lLI1.jpg](https://s21.ax1x.com/2024/03/16/pF2lLI1.jpg)

该服务提供了三类方法：获取ALV报表选择屏幕参数、获取ALV报表数据、获取底表数据，具体请求示例请查看下面的详细介绍。
## 获取ALV报表选择屏幕参数

请求地址：[http://vhcala4hci:50000/sap/bc/zsicf_bi?tcode=[tcode]](http://vhcala4hci:50000/sap/bc/zsicf_bi?tcode=%5Btcode%5D%5D "获取ALV报表选择屏幕参数")

请求方法：POST

Content-Type: "application/x-www-form-urlencoded"

请求体: action: "rsparams"

返回消息结构：

	{
		"rtype": "消息类型: S 成功,E 错误,W 警告,I 信息,A 中断",
		"rtmsg": "消息文本",
		"rsparams": [
			{
				"selname": "ABAP：SELECT-OPTION/PARAMETER 的名称",
				"kind": "ABAP/4: 选择类型，单选字段配置'P'，多选字段配置'S'",
				"sign": "ABAP/4: ID: I/E (包括/不包括值)",
				"option": "ABAP/4: 选择选项 (EQ/BT/CP/...)",
				"low": "ABAP：选择值（LOW 值，外部格式）",
				"high": "ABAP：选择值（HIGH 值，外部格式）"
			}
		]
	}

## 获取ALV报表数据
请求地址：[http://vhcala4hci:50000/sap/bc/zsicf_bi?tcode=[tcode]](http://vhcala4hci:50000/sap/bc/zsicf_bi?tcode=%5Btcode%5D%5D "获取ALV报表数据")

请求方法：POST

content-type: "application/json;charset=utf-8"

请求体:

	[
		{
			"selname": "ABAP：SELECT-OPTION/PARAMETER 的名称",
			"kind": "ABAP/4: 选择类型，单选字段配置'P'，多选字段配置'S'",
			"sign": "ABAP/4: ID: I/E (包括/不包括值)",
			"option": "ABAP/4: 选择选项 (EQ/BT/CP/...)",
			"low": "ABAP：选择值（LOW 值，外部格式）",
			"high": "ABAP：选择值（HIGH 值，外部格式）"
		}
	]

返回消息结构：

	{
		"rtype": "消息类型: S 成功,E 错误,W 警告,I 信息,A 中断",
		"rtmsg": "消息文本",
		"mapping": [
			{
				"fieldname": "字段名",
				"scrtext_s": "短字段描述",
				"scrtext_m": "中字段描述",
				"scrtext_l": "长字段描述"
			}
		],
		"data": [
			{
				"字段名": "字段值"
			}
		]
	}

## 获取底表数据
请求地址：[http://vhcala4hci:50000/sap/bc/zsicf_bi?tabname=[tabname]](http://vhcala4hci:50000/sap/bc/zsicf_bi?tabname=%5Btabname%5D%5D "获取底表数据")

请求方法：POST

返回消息结构：

	{
		"rtype": "消息类型: S 成功,E 错误,W 警告,I 信息,A 中断",
		"rtmsg": "消息文本",
		"mapping": [
			{
				"fieldname": "字段名",
				"scrtext_s": "短字段描述",
				"scrtext_m": "中字段描述",
				"scrtext_l": "长字段描述"
			}
		],
		"data": [
			{
				"字段名": "字段值"
			}
		]
	}

## 后续计划
1. 取底表数据添加OPEN SQL查询条件
2. 想得到再说吧  
  
## 联系我
邮箱：[weikj@foxmail.com](mailto:weikj@foxmail.com "kkw")
