package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.pile.base.domain.service.LocalAuthListService;
import com.autel.cloud.pile.base.dto.LocalListInformationDTO;
import com.autel.cloud.pile.base.dto.SaveAuthListDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.LocalAuthListEntity;
import com.autel.cloud.pile.base.vo.GetAllAuthListVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import java.util.List;

@RestController
@RequestMapping("/localAuthList")
@Api(tags = "本地白名单")
@Slf4j
@Validated
public class LocalAuthListController {
    @Resource
    private LocalAuthListService localAuthListService;

    @ApiOperation("根据桩sn查询白名单列表")
    @GetMapping("/getAllAuthListBySellerId")
    public Result<List<LocalAuthListEntity>> getAllAuthListBySellerId (@RequestParam("sn") String sn,@RequestParam("sellerId") Long sellerId) {
        return Result.ofSucceed(localAuthListService.getAllAuthListBySellerId(sn,sellerId));
    }

    @ApiOperation("保存白名单列表")
    @PostMapping("/save")
    public Result<Boolean> saveAuthList(@RequestBody SaveAuthListDTO saveAuthListDTO) {
        return Result.ofSucceed(localAuthListService.saveAuthList(saveAuthListDTO));
    }

    @ApiOperation("白名单列表")
    @GetMapping("/getAllAuthList")
    public Result<List<GetAllAuthListVO>> getAllAuthList (@RequestParam("sn") String sn) {
        Long sellerId = LoginUserHolder.getLoginUser().getPayload().getSellerId();
        return Result.ofSucceed(localAuthListService.getAllAuthList(sn,sellerId));
    }

    @ApiOperation("白名单详情")
    @GetMapping("/getDetail/{id}")
    public Result<LocalAuthListEntity> getDetail (@PathVariable Long id) {
        return Result.ofSucceed(localAuthListService.getDetail(id));
    }

    @ApiOperation("编辑白名单")
    @GetMapping("/update")
    public Result<Boolean> updateAuthList (LocalListInformationDTO localListInformationDTO) {
        return Result.ofSucceed(localAuthListService.updateAuthList(localListInformationDTO));
    }

    @ApiOperation("删除白名单")
    @GetMapping("/deleted")
    public Result<Boolean> deletedAuthList (@RequestParam("id") Long id,@RequestParam("sn") String sn) {
        return Result.ofSucceed(localAuthListService.deletedAuthList(id,sn));
    }

    @ApiOperation("下发该商家底下所有白名单列表")
    @PostMapping("/sendAllAuthList")
    public Result<Boolean> sendAllAuthList(@RequestBody SaveAuthListDTO saveAuthListDTO) {
        return Result.ofSucceed(localAuthListService.sendAllAuthList(saveAuthListDTO));
    }
}
