package org.im97mori.ble.android.peripheral.ui.device.setting.u180a;

import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.DeviceInformationServiceSettingActivityBinding;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a23.SystemIdLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a24.ModelNumberStringLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a29.ManufacturerNameStringLauncherContract;
import org.im97mori.ble.android.peripheral.utils.MockitoViewModelProvider;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class DeviceInformationServiceSettingActivity extends AppCompatActivity {

    private DeviceInformationServiceSettingViewModel mViewModel;

    private final ActivityResultLauncher<String> mStartManufacturerNameStringSettingActivity
            = registerForActivityResult(new ManufacturerNameStringLauncherContract(), result -> mViewModel.setManufacturerNameStringDataJson(result));

    private final ActivityResultLauncher<String> mStartModelNumberStringSettingActivity
            = registerForActivityResult(new ModelNumberStringLauncherContract(), result -> mViewModel.setModelNumberStringDataJson(result));

    private final ActivityResultLauncher<String> mStartSystemIdSettingActivity
            = registerForActivityResult(new SystemIdLauncherContract(), result -> mViewModel.setSystemIdDataJson(result));

    private DeviceInformationServiceSettingActivityBinding mBinding;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new MockitoViewModelProvider(this).get(DeviceInformationServiceSettingViewModel.class);

        mBinding = DeviceInformationServiceSettingActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        mViewModel.observeHasManufacturerNameStringDataJson(this, mBinding.manufacturerNameStringCardView::setChecked);
        mViewModel.observeHasModelNumberStringDataJson(this, mBinding.modelNumberStringCardView::setChecked);
        mViewModel.observeHasSystemIdDataJson(this, mBinding.systemIdCardView::setChecked);
        mViewModel.observeIsSystemIdSupported(this, check -> {
            mBinding.isSystemIdSupported.setChecked(check);
            mBinding.systemIdCardView.setVisibility(check ? View.VISIBLE : View.GONE);
        });

        mViewModel.observeManufacturerNameString(this, mBinding.manufacturerNameString::setText);
        mViewModel.observeModelNumberString(this, mBinding.modelNumberString::setText);
        mViewModel.observeManufacturerIdentifier(this, mBinding.manufacturerIdentifier::setText);
        mViewModel.observeOrganizationallyUniqueIdentifier(this, mBinding.organizationallyUniqueIdentifier::setText);

        mBinding.isSystemIdSupported.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateIsSystemIdSupported(isChecked));

        mBinding.manufacturerNameStringSettingButton.setOnClickListener(v -> mStartManufacturerNameStringSettingActivity.launch(mViewModel.getManufacturerNameStringDataJson()));
        mBinding.modelNumberStringSettingButton.setOnClickListener(v -> mStartModelNumberStringSettingActivity.launch(mViewModel.getModelNumberStringDataJson()));
        mBinding.systemIdSettingButton.setOnClickListener(v -> mStartSystemIdSettingActivity.launch(mViewModel.getSystemIdDataJson()));

        mBinding.topAppBar.setOnMenuItemClickListener(this::onOptionsItemSelected);
    }

    @Override
    protected void onStart() {
        super.onStart();
        mViewModel.observeSetup(getIntent()
                , () -> mBinding.rootContainer.setVisibility(View.VISIBLE)
                , throwable -> LogUtils.stackLog(throwable.getMessage()));
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        boolean result;
        if (item.getItemId() == R.id.save) {
            mViewModel.observeSave(intent -> {
                setResult(RESULT_OK, intent);
                finish();
            }, throwable -> Toast.makeText(this, throwable.getMessage(), Toast.LENGTH_SHORT).show());
            result = true;
        } else {
            result = super.onOptionsItemSelected(item);
        }
        return result;
    }

}
