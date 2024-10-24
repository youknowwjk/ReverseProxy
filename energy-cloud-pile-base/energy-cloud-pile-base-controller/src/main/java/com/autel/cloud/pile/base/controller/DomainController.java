package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.DomainService;
import com.autel.cloud.pile.base.dto.DomainInfoDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.DomainEntity;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;

/**
 * @author A20019
 * @since 2022/3/21 10:32
 */
@RestController
@RequestMapping("/domain")
@Api(value = "域名管理API", tags = "域名管理API")
@Validated
public class DomainController {

    @Resource
    private DomainService domainService;

    @Value("${server.url}")
    private String domainUrl;

    @PostMapping("/getLogo")
    @ApiOperation(value = "根据域名获取logo图片")
    public Result<String> getDomainLogo(@RequestBody DomainInfoDTO domainInfo) {

        return Result.ofSucceed(domainService.getDomainLogo(domainInfo));

    }

    @PostMapping("/add")
    @ApiOperation(value = "添加域名图片数据")
    public Result<Boolean> addDomainInfo(@RequestBody DomainEntity domain) {
        return Result.ofSucceed(domainService.addDomainInfo(domain));
    }

    @GetMapping("/get")
    @ApiOperation(value = "获取当前域名地址")
    public Result<String> getCurrentDomain() {
        return Result.ofSucceed(domainUrl);
    }
}
