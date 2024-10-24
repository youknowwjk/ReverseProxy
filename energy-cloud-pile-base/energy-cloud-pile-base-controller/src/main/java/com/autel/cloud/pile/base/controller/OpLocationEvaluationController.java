package com.autel.cloud.pile.base.controller;


import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpLocationEvaluationService;
import com.autel.cloud.pile.base.dto.OpLocationEvaluationDTO;
import com.autel.cloud.pile.base.vo.OpLocationEvaluationVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@Log4j2
@RestController
@RequestMapping("/opLocation/evaluation")
@Api(tags = "场站评价")
@Validated
public class OpLocationEvaluationController {
    @Autowired
    private OpLocationEvaluationService opLocationEvaluationService;


    @PostMapping("/page")
    @ApiOperation(value = "分页查询", notes = "分页查询")
    public Result<Page<OpLocationEvaluationVO>> page(@RequestBody OpLocationEvaluationDTO opLocationEvaluationDTO) {
        return opLocationEvaluationService.page(opLocationEvaluationDTO);
    }
}

