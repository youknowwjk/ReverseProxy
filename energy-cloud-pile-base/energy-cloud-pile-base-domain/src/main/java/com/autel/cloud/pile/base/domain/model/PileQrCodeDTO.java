package com.autel.cloud.pile.base.domain.model;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * 批量下载二维码页面桩信息
 *
 * @author A22598
 * @date 2023/11/24
 */
@Data
public class PileQrCodeDTO {
    @ApiModelProperty("桩名称")
    String name;
    @ApiModelProperty("桩SN")
    String pileSn;
    @ApiModelProperty("场站名称")
    String locationName;
    @ApiModelProperty("桩的充电枪类型列表")
    List<Integer> gunTypeCode;
    @ApiModelProperty("远程启动电话")
    String remoteStartPhoneNumber;
    @ApiModelProperty("桩识别码")
    private String identificationCode;
}
