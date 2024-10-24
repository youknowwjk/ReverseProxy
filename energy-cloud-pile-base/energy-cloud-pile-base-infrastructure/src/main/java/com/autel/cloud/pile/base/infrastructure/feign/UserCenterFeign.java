package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.app.UserCountryUnitInfoDto;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

/**
 * @author A22203
 * @Description
 * @Date 2022/5/5 14:51
 */
@FeignClient("user-center-app")
public interface UserCenterFeign {
    @GetMapping("/country/unit/{id}")
    @ApiOperation(value = "查询用户信息")
    Result<UserCountryUnitInfoDto> getUnit(@PathVariable("id") String userId);
}
