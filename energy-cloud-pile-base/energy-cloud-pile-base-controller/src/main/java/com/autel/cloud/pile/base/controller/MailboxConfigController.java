package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.Log;
import com.autel.cloud.infrastructure.sysconfig.log.enums.BusinessType;
import com.autel.cloud.pile.base.domain.service.MailboxConfigService;
import com.autel.cloud.pile.base.dto.MailboxConfigDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@Api(tags = "邮箱配置")
@RestController
@RequestMapping(value = "/mailbox/config")
@Validated
public class MailboxConfigController {

    private final MailboxConfigService mailboxConfigService;


    public MailboxConfigController(MailboxConfigService mailboxConfigService) {
        this.mailboxConfigService = mailboxConfigService;
    }

    @Log(title = "新增邮箱", businessType = BusinessType.INSERT, code = "50201416")
    @PostMapping
    @ApiOperation(value = "新增邮箱")
    public Boolean addMailbox(@RequestBody MailboxConfigDTO mailboxConfigDTO) {
        return mailboxConfigService.addMailbox(mailboxConfigDTO);
    }

    @Log(title = "修改邮箱", businessType = BusinessType.UPDATE, code = "50201417")
    @PutMapping
    @ApiOperation(value = "修改邮箱")
    public Boolean updateMailbox(@RequestBody MailboxConfigDTO mailboxConfigDTO) {
        return mailboxConfigService.updateMailbox(mailboxConfigDTO);
    }

    @Log(title = "删除邮箱", businessType = BusinessType.DELETE, code = "50201418")
    @DeleteMapping(value = "/{id}")
    @ApiOperation(value = "删除邮箱")
    public Boolean delMailbox(@PathVariable Long id) {
        return mailboxConfigService.delMailbox(id);
    }

    @GetMapping(value = "/page")
    @ApiOperation(value = "分页查询邮箱")
    public PageVO<MailboxConfigDTO> selectMailboxPage(@RequestBody MailboxConfigDTO mailboxConfigDTO) {
        return mailboxConfigService.selectMailboxPage(mailboxConfigDTO);
    }

    @GetMapping(value = "/{operatorId}")
    @ApiOperation(value = "根据商户ID查询邮箱详情")
    public MailboxConfigDTO selectMailboxByOperatorId(@PathVariable Long operatorId) {
        return mailboxConfigService.selectMailboxByOperatorId(operatorId);
    }
}
