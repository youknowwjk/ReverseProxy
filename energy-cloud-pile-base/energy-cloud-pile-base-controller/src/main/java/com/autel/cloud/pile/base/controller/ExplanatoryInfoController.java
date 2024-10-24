package com.autel.cloud.pile.base.controller;

import com.autel.cloud.authoritymanager.dto.BaseMenuDto;
import com.autel.cloud.authoritymanager.feign.AuthorityManagerFeign;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.http.vo.PageReqVO;
import com.autel.cloud.pile.base.domain.service.ExplanatoryInfoService;
import com.autel.cloud.pile.base.dto.ExplanatoryInfoDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.ExplanatoryInfoMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ExplanatoryInfoEntity;
import com.autel.cloud.pile.base.vo.ExplanatoryInfoVO;
import com.autel.cloud.pile.base.vo.app.PageDTO;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.apache.commons.lang3.StringUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import java.util.List;

@RestController
@RequestMapping("/explanatoryInfo")
@Api(value = "说明信息管理", tags = "说明信息管理")
@Validated
public class ExplanatoryInfoController {
    @Resource
    ExplanatoryInfoService explanatoryInfoService;

    @Resource
    AuthorityManagerFeign authorityManagerFeign;

    @Resource
    ExplanatoryInfoMapper explanatoryInfoMapper;

    @GetMapping("/menu")
    @ApiOperation(value = "获取商家菜单")
    public Result<List<? extends BaseMenuDto>> menu(@RequestHeader(name = "Accept-Language",defaultValue = "zh-CN") String language) {
        return authorityManagerFeign.listMenu("Chargebusi",null, language);
    }

    @ApiOperation(value = "保存或更新配置")
    @PostMapping("/saveOrUpdate")
    public Result<ExplanatoryInfoVO> saveOrUpdate(@Validated @RequestBody ExplanatoryInfoDTO explanatoryInfoDTO) {
        return explanatoryInfoService.saveOrUpdate(explanatoryInfoDTO);
    }

    @ApiOperation(value = "查询所有已配置的页面")
    @PostMapping("/searchConfigPages")
    public Result<PageDTO<ExplanatoryInfoVO>> searchConfigPages(@Validated @RequestBody PageReqVO<ExplanatoryInfoDTO> explanatoryInfoSearchDTO, @RequestHeader(name = "Accept-Language",defaultValue = "zh-CN") String language) {
        return explanatoryInfoService.searchConfigPages(explanatoryInfoSearchDTO, language);
    }

    @ApiOperation(value = "分页查询配置", notes = "根据页面id(菜单ID)分组")
    @PostMapping("/page")
    public Result<PageDTO<ExplanatoryInfoVO>> page(@Validated @RequestBody PageReqVO<ExplanatoryInfoDTO> explanatoryInfoSearchDTO, @RequestHeader(value = "Accept-Language",defaultValue = "zh-CN") String language) {
        //语言
        if (StringUtils.isEmpty(explanatoryInfoSearchDTO.getData().getLanguage())) {
            explanatoryInfoSearchDTO.getData().setLanguage(language);
        }
        Result<PageDTO<ExplanatoryInfoVO>> page = explanatoryInfoService.page(explanatoryInfoSearchDTO);
        if (page.getData().getTotalCount() == 0 && explanatoryInfoSearchDTO.getPage() == 1) { // 没有的话就返回英文
            explanatoryInfoSearchDTO.getData().setLanguage("en-US");
            return explanatoryInfoService.page(explanatoryInfoSearchDTO);
        }
        return page;
    }

    @GetMapping("/updateSql")
    public Boolean updateSql(@RequestParam("sourceId") Long sourceId, @RequestParam("targetId") Long targetId){
        UpdateWrapper<ExplanatoryInfoEntity> updateWrapper = new UpdateWrapper<>();
        updateWrapper.set("menu_id", targetId).eq("menu_id", sourceId);
        return  explanatoryInfoMapper.update(null, updateWrapper) == 1;
    }
}
