package com.autel.cloud.pile.base.domain.data;

import com.alibaba.excel.annotation.ExcelProperty;
import com.alibaba.excel.annotation.write.style.ColumnWidth;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Date;

/**
 * @author A22203
 * @Description
 * @Date 2022/5/18 14:08
 */
@Data
public class UserOpLogData implements Serializable {

    @ApiModelProperty(value = "操作人员名称")
    @ExcelProperty(value = "操作人员名称")
    @ColumnWidth(20)
    private String operName;

    @ApiModelProperty(value = "操作模块")
    @ExcelProperty(value = "操作模块")
    @ColumnWidth(20)
    private String title;

    @ApiModelProperty(name = "业务类型 0=其它,1=新增,2=修改,3=删除,4=授权,5=导出,6=导入,7=强退,8=生成代码,9=清空数据")
    @ExcelProperty(value = "业务类型")
    @ColumnWidth(20)
    private String businessType;

    @ApiModelProperty(name = "操作类别 0=其它,1=后台用户,2=手机端用户")
    @ExcelProperty(value = "操作类别")
    @ColumnWidth(20)
    private Integer operatorType;

    @ApiModelProperty(name = "请求地址")
    @ExcelProperty(value = "请求地址")
    @ColumnWidth(20)
    private String operUrl;

    @ApiModelProperty(name = "操作地址")
    @ExcelProperty(value = "操作地址")
    @ColumnWidth(20)
    private String operIp;

    @ApiModelProperty(value = "局域网IP地址")
    @ExcelProperty(value = "局域网IP地址")
    @ColumnWidth(20)
    private String ipLan;

    /**
     * 请求参数
     */
    @ApiModelProperty(name = "请求方式")
    @ExcelProperty(value = "请求方式")
    @ColumnWidth(20)
    private String requestMethod;

    /**
     * 请求参数
     */
    @ApiModelProperty(name = "请求参数")
    @ExcelProperty(value = "请求参数")
    @ColumnWidth(20)
    private String operParam;

    /**
     * 返回参数
     */
    @ApiModelProperty(name = "返回参数")
    @ExcelProperty(value = "返回参数")
    @ColumnWidth(20)
    private String jsonResult;

    /**
     * 操作状态（0正常 1异常）
     */
    @ApiModelProperty(name = "状态 0=正常,1=异常")
    @ExcelProperty(value = "状态")
    @ColumnWidth(20)
    private Integer status;

    @ApiModelProperty(name = "错误消息")
    @ExcelProperty(value = "错误消息")
    @ColumnWidth(20)
    private String errorMsg;

    @ApiModelProperty(value = "操作时间 yyyy-MM-dd HH:mm:ss")
    @ExcelProperty(value = "操作时间")
    @ColumnWidth(20)
    private String operTime;

    @ApiModelProperty(value = "创建时间 yyyy-MM-dd HH:mm:ss")
    @ExcelProperty(value = "创建时间")
    @ColumnWidth(20)
    private String createTime;

    @ApiModelProperty(value = "操作日志")
    @ExcelProperty(value = "操作日志")
    @ColumnWidth(20)
    private String detail;
}
