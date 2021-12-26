package org.im97mori.ble.android.peripheral.ui.device.setting.u2902;

import static org.im97mori.ble.android.peripheral.utils.Utils.setTextDistinct;

import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.ViewModelProvider;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.ClientCharacteristicConfigurationSettingActivityBinding;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;
import org.im97mori.ble.android.peripheral.utils.AfterTextChangedTextWatcher;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class ClientCharacteristicConfigurationSettingActivity extends BaseActivity {

    private ClientCharacteristicConfigurationSettingViewModel mViewModel;

    private ClientCharacteristicConfigurationSettingActivityBinding mBinding;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new ViewModelProvider(this).get(ClientCharacteristicConfigurationSettingViewModel.class);

        mBinding = ClientCharacteristicConfigurationSettingActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        mViewModel.observeIsErrorResponse(this, check -> {
            mBinding.isErrorResponse.setChecked(check);
            mBinding.propertiesRadioGroup.setVisibility(check ? View.GONE : View.VISIBLE);
            mBinding.responseCode.setVisibility(check ? View.VISIBLE : View.GONE);
        });
        mBinding.isErrorResponse.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateIsErrorResponse(isChecked));

        mViewModel.observeResponseCode(this, charSequence -> setTextDistinct(mBinding.responseCodeEdit, charSequence));
        mViewModel.observeResponseCodeError(this, charSequence -> mBinding.responseCode.setError(charSequence));
        mBinding.responseCodeEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateResponseCode(editable)));

        mViewModel.observeResponseDelayError(this, charSequence -> mBinding.responseDelay.setError(charSequence));
        mViewModel.observeResponseDelay(this, charSequence -> setTextDistinct(mBinding.responseDelayEdit, charSequence));
        mBinding.responseDelayEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateResponseDelay(editable)));

        mViewModel.observeProperties(this, check -> mBinding.propertiesRadioGroup.check(check ? R.id.propertiesEnabledRadioButton : R.id.propertiesDisabledRadioButton));
        mBinding.propertiesRadioGroup.setOnCheckedChangeListener((group, checkedId) -> mViewModel.updateProperties(R.id.propertiesEnabledRadioButton == checkedId));

        mViewModel.observePropertiesDisabled(this, charSequence -> mBinding.propertiesDisabledRadioButton.setText(charSequence));
        mViewModel.observePropertiesEnabled(this, charSequence -> mBinding.propertiesEnabledRadioButton.setText(charSequence));

        mBinding.topAppBar.setOnMenuItemClickListener(this::onOptionsItemSelected);
    }

    @Override
    protected void onStart() {
        super.onStart();
        mDisposable.add(mViewModel.setup(getIntent())
                .subscribe(() -> mBinding.rootContainer.setVisibility(View.VISIBLE), throwable -> LogUtils.stackLog(throwable.getMessage())));
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        boolean result;
        if (item.getItemId() == R.id.save) {
            mDisposable.add(mViewModel.save()
                    .subscribe(intent -> {
                        setResult(RESULT_OK, intent);
                        finish();
                    }, throwable -> Toast.makeText(this, throwable.getMessage(), Toast.LENGTH_SHORT).show()));
            result = true;
        } else {
            result = super.onOptionsItemSelected(item);
        }
        return result;
    }

}
