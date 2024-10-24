package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.UserInfoDTO;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

/**
 * @author A22203
 * @Description
 * @Date 2022/5/5 14:51
 */
@FeignClient("saas-access-app")
public interface UserInfoFeign {
    @PostMapping("/user/get/username")
    @ApiOperation(value = "查询用户信息集合")
    public Result<List<UserInfoDTO>> getUserNameList(@RequestBody List<Long> ids);
}
