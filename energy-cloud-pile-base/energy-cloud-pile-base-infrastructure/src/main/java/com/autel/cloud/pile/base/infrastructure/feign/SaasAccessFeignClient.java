package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.vo.SellerVO;
import com.autel.cloud.pile.user.api.dto.AddDataReqDTO;
import com.autel.cloud.pile.user.api.dto.DeleteDataReqDTO;
import com.autel.cloud.pile.user.api.dto.MoveDataReqDTO;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

/**
 * Author:   A19011
 * Description: SaasAccessFeignClient
 * Date:     2022/5/12 11:14
 *
 * @Version 0.0.1-SNAPSHOT
 */
@FeignClient("saas-access-app")
public interface SaasAccessFeignClient {
    @GetMapping("/user/{id}")
    @ApiOperation(value = "根据id查询用户信息")
    Result<SellerVO> querySellerById(@PathVariable("id") Long id);

    @PostMapping("/data/add-node")
    @ApiOperation(value = "添加数据树节点")
    Result<Boolean> addNde(@RequestBody List<AddDataReqDTO> dataReqDTO);

    @PostMapping("/data/delete-node")
    @ApiOperation(value = "删除数据树节点")
    Result<Boolean> deleteNde(@RequestBody List<DeleteDataReqDTO> deleteDataReqDTO);

    @PostMapping("/data/move-node")
    @ApiOperation(value = "移动数据树节点")
    Result<Boolean> moveNde(@RequestBody MoveDataReqDTO moveDataReqDTO);
}
