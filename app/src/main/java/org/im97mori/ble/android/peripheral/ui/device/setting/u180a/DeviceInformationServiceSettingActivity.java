package org.im97mori.ble.android.peripheral.ui.device.setting.u180a;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.view.MenuProvider;

import androidx.lifecycle.HasDefaultViewModelProviderFactory;
import androidx.lifecycle.ViewModelProvider;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.DeviceInformationServiceSettingActivityBinding;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a23.SystemIdLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a24.ModelNumberStringLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a29.ManufacturerNameStringLauncherContract;
import org.im97mori.ble.android.peripheral.utils.AutoDisposeViewModelProvider;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

import javax.inject.Inject;
import java.util.function.Function;

@AndroidEntryPoint
public class DeviceInformationServiceSettingActivity extends AppCompatActivity {

    @Inject
    Function<HasDefaultViewModelProviderFactory, ViewModelProvider.Factory> viewModelProviderFactoryFunction;

    private DeviceInformationServiceSettingViewModel mViewModel;

    private final ActivityResultLauncher<byte[]> mStartSystemIdSettingActivity
            = registerForActivityResult(new SystemIdLauncherContract(), result -> mViewModel.setSystemIdData(result));

    private final ActivityResultLauncher<byte[]> mStartModelNumberStringSettingActivity
            = registerForActivityResult(new ModelNumberStringLauncherContract(), result -> mViewModel.setModelNumberStringData(result));

    private final ActivityResultLauncher<byte[]> mStartManufacturerNameStringSettingActivity
            = registerForActivityResult(new ManufacturerNameStringLauncherContract(), result -> mViewModel.setManufacturerNameStringData(result));

    private DeviceInformationServiceSettingActivityBinding mBinding;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new AutoDisposeViewModelProvider(this, viewModelProviderFactoryFunction.apply(this)).get(DeviceInformationServiceSettingViewModel.class);

        mBinding = DeviceInformationServiceSettingActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        mViewModel.observeIsSystemIdSupported(this, check -> {
            mBinding.isSystemIdSupported.setChecked(check);
            mBinding.systemIdCardView.setVisibility(check ? View.VISIBLE : View.GONE);
        });
        mViewModel.observeHasSystemIdData(this, mBinding.systemIdCardView::setChecked);
        mViewModel.observeHasModelNumberStringData(this, mBinding.modelNumberStringCardView::setChecked);
        mViewModel.observeHasManufacturerNameStringData(this, mBinding.manufacturerNameStringCardView::setChecked);

        mViewModel.observeManufacturerIdentifier(this, mBinding.manufacturerIdentifier::setText);
        mViewModel.observeOrganizationallyUniqueIdentifier(this, mBinding.organizationallyUniqueIdentifier::setText);
        mViewModel.observeModelNumberString(this, mBinding.modelNumberString::setText);
        mViewModel.observeManufacturerNameString(this, mBinding.manufacturerNameString::setText);

        mBinding.isSystemIdSupported.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateIsSystemIdSupported(isChecked));

        mBinding.systemIdSettingButton.setOnClickListener(v -> mStartSystemIdSettingActivity.launch(mViewModel.getSystemIdData()));
        mBinding.modelNumberStringSettingButton.setOnClickListener(v -> mStartModelNumberStringSettingActivity.launch(mViewModel.getModelNumberStringData()));
        mBinding.manufacturerNameStringSettingButton.setOnClickListener(v -> mStartManufacturerNameStringSettingActivity.launch(mViewModel.getManufacturerNameStringData()));

        mViewModel.observeSavedData(this, intent -> {
            setResult(RESULT_OK, intent);
            finish();
        });

        mBinding.topAppBar.addMenuProvider(new MenuProvider() {

            @Override
            public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                menu.findItem(R.id.save).setEnabled(mBinding.rootContainer.getVisibility() == View.VISIBLE);
            }

            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                boolean result = false;
                if (menuItem.getItemId() == R.id.save) {
                    mViewModel.save(throwable
                            -> Toast.makeText(DeviceInformationServiceSettingActivity.this
                            , throwable.getMessage()
                            , Toast.LENGTH_SHORT).show());
                    result = true;
                }
                return result;
            }
        });
    }

    @Override
    protected void onStart() {
        super.onStart();
        mViewModel.observeSetup(getIntent()
                , () -> {
                    mBinding.rootContainer.setVisibility(View.VISIBLE);
                    mBinding.topAppBar.invalidateMenu();
                }
                , throwable -> LogUtils.stackLog(throwable.getMessage()));
    }

}
