package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.config.ops.request.DownConfigMsgToOpsDTO;
import com.autel.cloud.pile.base.vo.pos.PileVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

/**
 * @Author A22599
 * @Date 2023/06/02
 * @Function  运维SDK API接口
 */
@FeignClient("ops-api")
public interface OpsApiClient {

    @PostMapping("/downMsg/config")
    @ApiOperation(value = "通过ACMP协议下发一条配置信息")
    Result<Boolean> downConfigMessage(@RequestBody DownConfigMsgToOpsDTO downConfigMsgToOpsDTO);

    @PostMapping("/selectorPile/getPileDropDownPageList")
    @ApiOperation(value = "分页查询充电桩序列号下拉列表", notes = "分页查询充电桩序列号下拉列表")
    Result<Page<PileVO>> getPileDropDownPageList(@RequestBody PageDTO pageDTO);
}
